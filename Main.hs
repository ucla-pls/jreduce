{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main where

import qualified Data.List             as L
import           Data.Monoid
import qualified Data.Set              as S
import qualified Data.Map              as M
import qualified Data.Vector           as V
import qualified Data.Text             as Text
import qualified Data.Text.IO          as Text
import           Data.List.Split
import           Data.Time
import           Data.Time.Clock.POSIX
import           System.Console.Docopt
import           System.Directory
import           System.Environment    (getArgs)
import           System.Exit
import           System.Timeout
import           System.IO
import           System.IO.Temp
import           System.Process

import           Control.Monad
import           Control.Monad.IO.Class

import           Control.Lens          hiding (argument)

-- import Debug.Trace

import           Jvmhs
import           Jvmhs.Analysis.Reduce
import           Control.Concurrent

patterns :: Docopt
patterns = [docopt|
jreduce version 0.0.1

Jreduce is a tool that can reduce a java program to a smaller program.
If a property have been specified then jreduce will try to reduce to
the smallest program that still uphold the property.

Usage:
  jreduce ( -h | --help )
  jreduce [-v | -vv | -vvv] [options] [<property>...]

Options:
  --cp <classpath>          The classpath to search for classess
  --stdlib                  Also include the stdlib (Don't do this)
  --jre <jre>               The location of the stdlib
  -W, --warn                Warn about missing classes to the stderr
  -o, --output <output>     Output folder or jar
  -r, --reductor <reductor> Reductor
  --progress <progress>     A file that will contain csv data for each
                            interation of the program
  -c, --core <core>         The core classes, that should not be removed
  --tmp <tmp>               The tempfolder default is the system tmp folder
  -K, --keep                Keep temporary folders around
  -t, --timout <timeout>    Timeout in miliseconds, default is 1000 ms
  -v                        Be more verbose
|]

-- | The config file dictates the execution of the program
data Config = Config
  { _cfgClassPath      :: ClassPath
  , _cfgOutput         :: Maybe FilePath
  , _cfgClasses        :: S.Set ClassName
  , _cfgUseStdlib      :: Bool
  , _cfgWarn           :: Bool
  , _cfgVerbose        :: Int
  , _cfgReductor       :: String
  , _cfgTempFolder     :: FilePath
  , _cfgJre            :: Maybe FilePath
  , _cfgProgressFile   :: Maybe FilePath
  , _cfgProperty       :: [String]
  , _cfgKeepTempFolder :: Bool
  , _cfgTimeout        :: Int
  } deriving (Show)

makeLenses 'Config

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns

parseConfig :: Arguments -> IO Config
parseConfig args = do
  classnames <- readClassNames
  tmpfolder <- getCanonicalTemporaryDirectory
  return $ Config
    { _cfgClassPath =
        case concatMap splitClassPath $ getAllArgs args (longOption "cp") of
          [] -> ["."]
          as -> as
    , _cfgOutput = getArg args (shortOption 'o')
    , _cfgClasses = classnames
    , _cfgWarn = isPresent args (longOption "warn")
    , _cfgReductor = getArgWithDefault args "gdd" (longOption "reductor")
    , _cfgUseStdlib = isPresent args (longOption "stdlib")
    , _cfgJre = getArg args (longOption "jre")
    , _cfgVerbose = getArgCount args (shortOption 'v')
    , _cfgProgressFile = getArg args (longOption "progress")
    , _cfgProperty = getAllArgs args (argument "property")
    , _cfgTempFolder = getArgWithDefault args tmpfolder (longOption "tmp")
    , _cfgKeepTempFolder = isPresent args (longOption "keep")
    , _cfgTimeout = read $ getArgWithDefault args "1000" (longOption "timeout")
    }

  where
    classnames' = getAllArgs args $ longOption "core"
    readClassNames :: IO (S.Set ClassName)
    readClassNames = do
      names <- fmap concat . forM classnames' $ \cn ->
        case cn of
          '@':filename -> lines <$> readFile filename
          _ -> return [cn]
      return . S.fromList . map strCls $ names
    -- splitReductions = do
    --   case getArg args (shortOption 'r') of
    --     Nothing -> return []
    --     Just s  -> return $ splitOn ":" s


type Property m = (String -> m Bool)

setupProperty ::
  (MonadClassPool m, MonadIO m)
  => Config
  -> m (Property m)
setupProperty cfg = do
  case cfg^.cfgProgressFile of
    Just pf ->
      liftIO . withFile pf WriteMode $ \h -> do
        hPutStrLn h "step,time,classes,interfaces,impls,methods,success"
    Nothing ->
      return ()
  case cfg^.cfgProperty of
    cmd:cmdArgs -> do
      tmp <- liftIO $ do
        createDirectoryIfMissing True (cfg^.cfgTempFolder)
        createTempDirectory (cfg^.cfgTempFolder) "jreduce"
      sem <- liftIO $ newMVar 0
      return $ runProperty cmd cmdArgs tmp sem
    _ ->
      return $ const (return True)
  where
    pad m c xs = replicate (m - length xs) c ++ xs

    -- runProperty :: String -> [String] -> FilePath -> MVar Int -> Property m
    runProperty cmd cmdArgs tmp sem name = do
      iteration <- liftIO $ modifyMVar sem (\i -> return (i + 1, i))
      let
        iterationname = name -- ++ "-" ++ pad 8 '0' (show iteration)
        outputfolder = tmp ++ "/" ++ iterationname
      liftIO $ createDirectoryIfMissing True outputfolder
      time cfg ("Saving classes to " ++ outputfolder) $
        saveAllClasses outputfolder
      res <- time cfg "Running property" . liftIO $
         withCreateProcess (
           (proc cmd (cmdArgs ++ [outputfolder]))
              { std_in = NoStream
              , std_out = if cfg^.cfgVerbose > 1 then Inherit else NoStream
              , std_err = if cfg^.cfgVerbose > 1 then Inherit else NoStream
              }) $
            \_ _ _ ph -> do
              maybeCode <- timeout (1000 * cfg^.cfgTimeout) (waitForProcess ph)
              case maybeCode of
                Nothing -> do
                  terminateProcess ph
                  return False
                Just ec ->
                  return $ ec == ExitSuccess
      info cfg $ "Property was " ++ show res
      logProgress cfg iterationname res
      liftIO $
        if (cfg^.cfgKeepTempFolder) then
          putStrLn tmp
        else do
          removeDirectoryRecursive tmp
      return res

    logProgress cfg name succ =
      case cfg^.cfgProgressFile of
        Just pf -> do
          clss <- allClasses
          let numClss = length clss
              (Sum numInterfaces, Sum numImpls, Sum numMethods) =
                foldMap (\cls -> ( Sum (if isInterface cls then 1 :: Int else 0)
                          , Sum (cls^.classInterfaces.to S.size)
                          , Sum (cls^.classMethods.to M.size)
                          )) clss
          liftIO $ do
            time <- getPOSIXTime
            withFile pf AppendMode $ \h -> do
              hPutStrLn h $ L.intercalate ","
                [ name
                , show $ (realToFrac time :: Double)
                , show $ numClss
                , show $ numInterfaces
                , show $ numImpls
                , show $ numMethods
                , show $ succ
                ]
        Nothing ->
          return ()

classClosure ::
  (MonadClassPool m, MonadIO m)
  => Config
  -> Property m
  -> m ()
classClosure cfg property = do
  (found, missing) <-
    time cfg "Running class closure" $
      computeClassClosure $ cfg^.cfgClasses
  info cfg $ "Found " ++ show (S.size found) ++ " required classes and "
          ++ show (S.size missing) ++ " missing classes."

  b <- cproperty "class-closure" found
  if b
    then
      onlyClasses found
    else do
      let red = cfg^.cfgReductor
          prop = cproperty red . (S.toList found ++)
      clss <- allClassNames
      keep <- (S.toList found ++) <$> case red of
        "ddmin" ->
          ddmin prop clss
        "gddmin" -> do
          gr <- mkClassGraph (S.toList $ S.fromList clss `S.difference` found)
          gddmin prop gr
        "gdd" -> do
          gr <- mkClassGraph (S.toList $ S.fromList clss `S.difference` found)
          igdd prop gr
        _ -> error $ "Unknown reductor " ++ show red
      onlyClasses keep

  void $ property "after-class-closure"
  where
    cproperty name clss =
      cplocal $ do
        onlyClasses clss
        property name

methodClosure ::
  (MonadClassPool m, MonadIO m)
  => Config
  -> Property m
  -> m ()
methodClosure cfg property = do
  info cfg "Running method closure ..."
  hry <- time cfg "Calculate hierarchy" $
     calculateHierarchy =<< allClassNames

  clss <- allClasses
  mths <- clss ^!! folded . classMethodIds

  rmths <- time cfg "Finding required methods" $
    filterM (isMethodRequired hry) mths

  info cfg $ "Found " ++ show (length rmths) ++ "/"
      ++ show (length mths) ++ " required methods"

  found <-
    time cfg "Compute method closure" $
     computeMethodClosure hry (S.fromList rmths)

  info cfg $ "Found " ++ show (S.size found) ++ " required methods after closure"

  b <- mproperty "method-closure" found
  if b
    then
       reduceto found
    else do
      let red = cfg^.cfgReductor
          prop = mproperty red . (S.toList found ++)
      keep <- (S.toList found ++) <$> case red of
        "ddmin" ->
          ddmin prop mths
        "gddmin" -> do
          gr <- mkCallGraph hry (S.toList $ S.fromList mths `S.difference` found)
          gddmin prop gr
        "gdd" -> do
          gr <- mkCallGraph hry (S.toList $ S.fromList mths `S.difference` found)
          igdd prop gr
        _ -> error $ "Unknown reductor " ++ show red
      reduceto keep

  void $ property "after-method-closure"
  where
    reduceto methods = do
      let clsmp = M.fromListWith (S.union)
            $ methods ^.. folded
            . to (\c -> (c^.inClassName, S.singleton $ c ^.inId ))
      modifyClasses $ \c ->
        let methods = clsmp ^. at (c^.className) . folded in
        Just (c & classMethods %~ flip M.restrictKeys methods)

    mproperty name methods = do
      cplocal $ do
        reduceto methods
        property name

runJReduce :: Config -> IO ()
runJReduce cfg = do

  classreader <- time cfg "Preloading classes ..." $
    preload =<< createClassLoader cfg

  cnt <- length <$> classes classreader
  info cfg $ "Found " ++ show cnt ++ " classes."

  void . flip runCachedClassPool classreader $ do
    property <- setupProperty cfg

    -- Test if the property have been correctly setup
    b <- property "initial"
    when (not b) $ fail "property failed on initial classpath"

    -- Run the class closure
    classClosure cfg property

    -- Run the method closure
    methodClosure cfg property

    property "final"
    case cfg^.cfgOutput of
      Just fp -> do
        time cfg "Saving classes..." $
          saveAllClasses fp
      Nothing ->
        info cfg "Doing nothing..."

  where
    handleFailedToLoad [] = return ()
    handleFailedToLoad errs = do
      hPutStrLn stderr "Could not load the following classes"
      mapM_ (\e -> hPutStr stderr "  - " >> hPutStrLn stderr (show e)) errs

info :: MonadIO m => Config -> String -> m ()
info cfg =
  when (cfg^.cfgVerbose > 0) .
    liftIO . hPutStrLn stderr

time :: MonadIO m => Config -> String -> m a -> m a
time cfg str m
  | cfg^.cfgVerbose > 0 = do
    t <- liftIO $ do
      hPutStr stderr str
      hPutStr stderr "... "
      getPOSIXTime
    a <- m
    liftIO $ do
      t' <- getPOSIXTime
      hPutStrLn stderr $ "done [" ++ show (t' - t) ++ "]"
    return a
  | otherwise = m
main :: IO ()
main = do
  args' <- parseArgs patterns <$> getArgs
  case args' of
    Right args
      | isPresent args (longOption "help")
        || isPresent args (shortOption 'h') ->
          exitWithUsage patterns
      | otherwise -> do
          cfg <- parseConfig args
          runJReduce cfg
    Left msg -> do
      print msg

-- | Create a class loader from the config
createClassLoader :: Config -> IO ClassLoader
createClassLoader cfg
  | cfg ^. cfgUseStdlib =
    case cfg ^. cfgJre of
      Nothing ->
        fromClassPath (cfg ^. cfgClassPath)
      Just jre ->
        fromJreFolder (cfg ^. cfgClassPath) jre
  | otherwise =
    return $ ClassLoader [] [] (cfg ^. cfgClassPath)


{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main where

import qualified Data.IntSet as IS
import qualified Data.List as L
import           Data.List.Split
import qualified Data.Map as M
import           Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Time
import           Data.Time.Clock.POSIX
import qualified Data.Vector as V
import           Prelude hiding (log)
import           System.Console.Docopt
import           System.Directory
import           System.Environment (getArgs)
import           System.Exit
import           System.IO
import           System.IO.Temp
import           System.Process
import           System.Timeout

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Control.Reduce

import           Control.Lens hiding (argument)

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

data ReductorName
  = DDMin
  | VerifyDDMin
  | GraphDDMin
  deriving (Show, Eq)

parseReductorName str =
  case str of
    "ddmin" -> Right DDMin
    "ddmin:verify" -> Right VerifyDDMin
    "ddmin:graph" -> Right GraphDDMin
    _ -> Left $ "Unknown reductor: " ++ str

-- | The config file dictates the execution of the program
data Config = Config
  { _cfgClassPath      :: !ClassPath
  , _cfgOutput         :: !(Maybe FilePath)
  , _cfgClasses        :: !(S.Set ClassName)
  , _cfgUseStdlib      :: !Bool
  , _cfgWarn           :: !Bool
  , _cfgVerbose        :: !Int
  , _cfgReductor       :: !ReductorName
  , _cfgTempFolder     :: !FilePath
  , _cfgJre            :: !(Maybe FilePath)
  , _cfgProgressFile   :: !(Maybe FilePath)
  , _cfgProperty       :: ![String]
  , _cfgKeepTempFolder :: !Bool
  , _cfgTimeout        :: !Int
  , _cfgLoggingIndent  :: !Int
  } deriving (Show)

makeLenses 'Config


getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns

parseConfig :: Arguments -> IO Config
parseConfig args = do
  classnames <- readClassNames
  tmpfolder <- getCanonicalTemporaryDirectory
  reductor <- case parseReductorName $ getArgWithDefault args "ddmin" (longOption "reductor") of
    Left err -> error err
    Right name -> return name
  return Config
    { _cfgClassPath =
        case concatMap splitClassPath $ getAllArgs args (longOption "cp") of
          [] -> ["."]
          as -> as
    , _cfgOutput = getArg args (shortOption 'o')
    , _cfgClasses = classnames
    , _cfgWarn = isPresent args (longOption "warn")
    , _cfgReductor = reductor
    , _cfgUseStdlib = isPresent args (longOption "stdlib")
    , _cfgJre = getArg args (longOption "jre")
    , _cfgVerbose = getArgCount args (shortOption 'v')
    , _cfgProgressFile = getArg args (longOption "progress")
    , _cfgProperty = getAllArgs args (argument "property")
    , _cfgTempFolder = getArgWithDefault args tmpfolder (longOption "tmp")
    , _cfgKeepTempFolder = isPresent args (longOption "keep")
    , _cfgTimeout = read $ getArgWithDefault args "1000" (longOption "timeout")
    , _cfgLoggingIndent = 0
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
  (MonadClassPool m, MonadIO m, MonadReader Config m)
  => m (Property m)
setupProperty = do
  cfg <- ask
  case cfg^.cfgProgressFile of
    Just pf ->
      liftIO . writeFile pf $ "step,time,classes,interfaces,impls,methods,success\n"
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
    runProperty cmd cmdArgs tmp sem name = time "Running property" $ do
      cfg <- ask
      iteration <- liftIO $ modifyMVar sem (\i -> return (i + 1, i))
      let
        iterationname = name -- ++ "-" ++ pad 8 '0' (show iteration)
        outputfolder = tmp ++ "/" ++ iterationname
      liftIO $ createDirectoryIfMissing True outputfolder
      time ("Saving classes to " ++ outputfolder) $
        saveAllClasses outputfolder
      res <- time "Invoking property" . liftIO $
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
      info $ "Property was " ++ show res
      logProgress cfg iterationname res
      liftIO $
        if cfg^.cfgKeepTempFolder then
          putStrLn tmp
        else
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
            appendFile pf $ L.intercalate ","
                [ name
                , show (realToFrac time :: Double)
                , show numClss
                , show numInterfaces
                , show numImpls
                , show numMethods
                , show succ
                ] ++ "\n"
        Nothing ->
          return ()

classClosure ::
  (MonadClassPool m, MonadIO m, MonadReader Config m)
  => Property m
  -> m ()
classClosure property = time "class-closure" $ do
  (found, missing) <-
    time "Running class closure" $
      computeClassClosure =<< view cfgClasses
  info $ "Found " ++ show (S.size found) ++ " required classes and "
          ++ show (S.size missing) ++ " missing classes."

  clss <- allClassNames
  graph <- mkClassGraph (S.toList $ S.fromList clss `S.difference` found)
  keep <- reduce cproperty graph
  case keep of
    Just clss' -> onlyClasses clss'
    Nothing -> do
      info $ "Could not statisfy predicate"
      onlyClasses found

  void $ property "after-class-closure"
  where
    cproperty name clss =
      cplocal $ do
        onlyClasses clss
        property $ "class-closure-" ++ name

reduce ::
  (MonadReader Config m, Ord a)
  => (String -> Predicate [a] m)
  -> Graph a e
  -> m (Maybe [a])
reduce prop graph = do
  red <- view cfgReductor
  case red of
    DDMin ->
      ddmin (prop "ddmin") (graph ^.. grNodes)
    VerifyDDMin -> do
      let propv xs = do
            if isClosedIn xs graph
              then prop "ddmin:verify" xs
              else return False
      ddmin propv (graph ^.. grNodes)
    GraphDDMin -> do
      let
        sets = closures graph
        unset = map (^?! toLabel graph . _Just) . IS.toList . IS.unions
        propi = prop "ddmin:graph" . unset
      fmap unset <$> ddmin propi sets

-- methodClosure ::
--   (MonadClassPool m, MonadIO m)
--   => Config
--   -> Property m
--   -> m ()
-- methodClosure cfg property = do
--   info cfg "Running method closure ..."
--   hry <- time cfg "Calculate hierarchy" $
--      calculateHierarchy =<< allClassNames

--   clss <- allClasses
--   mths <- clss ^!! folded . classMethodIds

--   -- rmths <- time cfg "Finding required methods" $
--   --   filterM (isMethodRequired hry) mths
--   let rmths = []

--   info cfg $ "Found " ++ show (length rmths) ++ "/"
--       ++ show (length mths) ++ " required methods"

--   found <-
--     time cfg "Compute method closure" $
--      computeMethodClosure hry (S.fromList rmths)

--   info cfg $ "Found " ++ show (S.size found) ++ " required methods after closure"

--   b <- mproperty "method-closure" found
--   if b
--     then
--        reduceto found
--     else do
--       let red = cfg^.cfgReductor
--           prop = mproperty ("method-closure-" ++ red) . (S.toList found ++)
--       keep <- (S.toList found ++) <$> case red of
--         "ddmin" ->
--           ddmin prop mths
--         "gddmin" -> do
--           gr <- mkCallGraph hry (S.toList $ S.fromList mths `S.difference` found)
--           gddmin prop gr
--         "gdd" -> do
--           gr <- mkCallGraph hry (S.toList $ S.fromList mths `S.difference` found)
--           igdd prop gr
--         _ -> error $ "Unknown reductor " ++ show red
--       reduceto keep

--   void $ property "after-method-closure"
--   where
--     reduceto methods = do
--       let clsmp = M.fromListWith (S.union)
--             $ methods ^.. folded
--             . to (\c -> (c^.inClassName, S.singleton $ c ^.inId ))
--       modifyClasses $ \c ->
--         let methods = clsmp ^. at (c^.className) . folded in
--         Just (c & classMethods %~ flip M.restrictKeys methods)

--     mproperty name methods = do
--       cplocal $ do
--         reduceto methods
--         property name

runJReduce :: RIO Config ()
runJReduce = time "jreduce" $ do
  classreader <- time "Preloading classes" $ do
    cls <- createClassLoader
    liftIO $ preload cls

  cnt <- liftIO $ length <$> classes classreader
  info $ "Found " ++ show cnt ++ " classes."

  void . flip runCachedClassPoolT classreader $ do
    property <- setupProperty

    -- Test if the property have been correctly setup
    b <- property "initial"
    unless b $ fail "property failed on initial classpath"

    -- Run the class closure
    classClosure property

    -- -- Run the method closure
    -- methodClosure cfg property

    property "final"
    view cfgOutput >>= \case
      Just fp ->
        time "Saving classes" $ saveAllClasses fp
      Nothing ->
        info "Does not save classes"

  where
    handleFailedToLoad [] = return ()
    handleFailedToLoad errs = do
      log "Could not load the following classes"
      mapM_ (\e -> log $ "  - " ++ show e) errs

info :: (MonadReader Config m, MonadIO m) => String -> m ()
info str = do
  v <- view cfgVerbose
  when (v > 0) $ log str

time :: (MonadReader Config m , MonadIO m) => String -> m a -> m a
time str m = do
  v <- view cfgVerbose
  if v > 0
    then do
      log str
      t <- liftIO getPOSIXTime
      a <- local (cfgLoggingIndent %~ (+1)) m
      t' <- liftIO getPOSIXTime
      log $ "done [" ++ show (t' - t) ++ "]"
      return a
    else m

log :: (MonadReader Config m, MonadIO m) => String -> m ()
log msg = do
  i <- view cfgLoggingIndent
  liftIO . hPutStrLn stderr $ concat (replicate i "| ") ++ msg

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
          runRIO runJReduce cfg
    Left msg ->
      print msg

-- | Create a class loader from the config
createClassLoader :: RIO Config ClassLoader
createClassLoader = RIO $ \cfg -> do
  let cp = cfg ^. cfgClassPath
  if cfg ^. cfgUseStdlib
    then
      case cfg ^. cfgJre of
        Nothing ->
          liftIO $ fromClassPath cp
        Just jre ->
          liftIO $ fromJreFolder cp jre
    else
      return $ ClassLoader [] [] cp

newtype RIO a b = RIO { runRIO :: a -> IO b }
  deriving (Functor)

instance Applicative (RIO a) where
  pure = RIO . const . pure
  mf <*> mx =
    RIO (\a -> runRIO mf a <*> runRIO mx a)

instance Monad (RIO a) where
  m >>= f = RIO (\a -> do x <- runRIO m a; runRIO (f x) a)

instance MonadReader a (RIO a) where
  reader f = RIO $ return . f
  local fr m = RIO (runRIO m . fr)

instance MonadIO (RIO a) where
  liftIO m = RIO (const m)

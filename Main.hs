{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main where

import qualified Data.List             as L
import           Data.Monoid
import qualified Data.Set              as S
import qualified Data.Text             as Text
import qualified Data.Text.IO          as Text
import           System.Console.Docopt
import           System.Directory
import           System.Environment    (getArgs)
import           System.Exit
import           System.IO
import           System.IO.Temp
import           System.Process

import           Control.Monad

import           Control.Lens          hiding (argument)
import           Jvmhs

patterns :: Docopt
patterns = [docopt|
jreduce version 0.0.1

Usage:
  jreduce ( -h | --help )
  jreduce [options] [-c <core>...] [-o <output>] [<property>...]

Options:
  --cp=<classpath>       The classpath to search for classess
  --stdlib               Also include the stdlib (Don't do this)
  --jre=<jre>            The location of the stdlib
  -W, --warn             Warn about missing classes to the stderr
  --progress=<progress>  A file that will contain csv data for each
                         interation of the program
  -c, --core <core>      The core classes, that should not be removed
  --tmp <tmp>            The tempfolder default is the system tmp folder
  -K, --keep             Keep temporary folders around
|]

-- | The config file dictates the execution of the program
data Config = Config
  { _cfgClassPath      :: ClassPath
  , _cfgOutput         :: Maybe FilePath
  , _cfgClasses        :: S.Set ClassName
  , _cfgUseStdlib      :: Bool
  , _cfgWarn           :: Bool
  , _cfgTempFolder     :: FilePath
  , _cfgJre            :: Maybe FilePath
  , _cfgProgressFile   :: Maybe FilePath
  , _cfgProperty       :: [String]
  , _cfgKeepTempFolder :: Bool
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
    , _cfgUseStdlib = isPresent args (longOption "stdlib")
    , _cfgJre = getArg args (longOption "jre")
    , _cfgProgressFile = getArg args (longOption "progress")
    , _cfgProperty = getAllArgs args (argument "property")
    , _cfgTempFolder = getArgWithDefault args tmpfolder (longOption "tmp")
    , _cfgKeepTempFolder = isPresent args (longOption "keep")
    }

  where
    classnames' = getAllArgs args $ longOption "core"
    readClassNames = do
      -- names <-
      --   if isPresent args (command "-")
      --   then do (classnames' ++) . lines <$> getContents
      --   else return classnames'
      return . S.fromList . map strCls $ classnames'


setupProgress :: Config -> IO ()
setupProgress cfg =
  case cfg^.cfgProgressFile of
    Just pf ->
      withFile pf WriteMode $ \h -> do
        hPutStrLn h "step,classes,interfaces,impls,methods,property"
    Nothing ->
      return ()

logProgress ::
  forall m t. (MonadClassPool m, MonadIO m, Foldable t)
  => Config
  -> t ClassName
  -> String
  -> m ()
logProgress cfg clss name =
  case cfg^.cfgProgressFile of
    Just pf -> do
      let numClss = length clss
      (Sum numInterfaces, Sum numImpls, Sum numMethods) <-
        clss ^! folded.load.to (
          \cls -> ( Sum (if isInterface cls then 1 :: Int else 0)
                  , Sum (cls^.classInterfaces.to length)
                  , Sum (cls^.classMethods.to length)
                  ))
      prop <- runProperty cfg clss
      liftIO $ withFile pf AppendMode $ \h -> do
        hPutStrLn h $ L.intercalate ","
          [ name
          , show $ numClss
          , show $ numInterfaces
          , show $ numImpls
          , show $ numMethods
          , show $ prop
          ]
    Nothing ->
      return ()

runProperty ::
  (MonadClassPool m, MonadIO m, Foldable t)
  => Config
  -> t ClassName
  -> m Bool
runProperty cfg clss =
  case cfg^.cfgProperty of
    cmd:cmdArgs -> do
      tmp <- liftIO $ do
        createDirectoryIfMissing True (cfg^.cfgTempFolder)
        createTempDirectory (cfg^.cfgTempFolder) "jreduce"
      dumpClassPool tmp clss
      res <- liftIO $ withCreateProcess (proc cmd (cmdArgs ++ [tmp])) $
        \_ _ _ ph -> do
          ec <- waitForProcess ph
          return $ ec == ExitSuccess
      liftIO (
        if (cfg^.cfgKeepTempFolder) then
          putStrLn tmp
        else do
          removeDirectoryRecursive tmp
        )
      return res
    _ -> return True

runJReduce :: Config -> IO ()
runJReduce cfg = do
  classreader <- preload =<< createClassLoader cfg
  clss <- map fst <$> classes classreader
  setupProgress cfg
  result <- flip runClassPool' (emptyState classreader) $ do
    logProgress cfg clss "-"
    (found, missing) <- computeClassClosure (cfg^.cfgClasses)
    logProgress cfg found "class-closure"
    return (found, missing)

  case result of
    Right ((found, missed), hs) -> do
      case cfg^.cfgOutput of
        Just fp ->
          savePartialClassPoolState fp found hs
        Nothing ->
          mapM_ (Text.putStrLn . view fullyQualifiedName) found
      when (cfg^.cfgWarn) $ do
        hPutStrLn stderr "Did not find these classes on the class path while reducing:"
        mapM_ ( Text.hPutStrLn stderr
              . Text.append "  - "
              . view fullyQualifiedName
              ) missed
    Left msg ->
      error $ show msg

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

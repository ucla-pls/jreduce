{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main where

import qualified Data.List             as L
import           Data.Monoid
import qualified Data.Set              as S
import qualified Data.Vector           as V
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
import           Control.Monad.IO.Class

import           Control.Lens          hiding (argument)
import           Jvmhs
import           Jvmhs.Analysis.Reduce

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
  forall m. (MonadClassPool m, MonadIO m)
  => Config
  -> String
  -> m ()
logProgress cfg name =
  case cfg^.cfgProgressFile of
    Just pf -> do
      clss <- allClasses
      let numClss = length clss
      (Sum numInterfaces, Sum numImpls, Sum numMethods) <-
        clss ^! folded.to (
          \cls -> ( Sum (if isInterface cls then 1 :: Int else 0)
                  , Sum (cls^.classInterfaces.to length)
                  , Sum (cls^.classMethods.to length)
                  ))
      prop <- runProperty cfg $ (^. className) <$> clss
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
      saveClasses tmp clss
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
  setupProgress cfg
  (errs, st) <- loadClassPoolState classreader

  handleFailedToLoad errs

  void . flip runClassPoolT st $ do
    clss <- allClassNames
    retcode <- runProperty cfg clss
    liftIO $ print retcode
    logProgress cfg "-"
    minVec <- ddmin (V.fromList clss) (runProperty cfg)
    liftIO $ print minVec
    (found, missing) <- computeClassClosure (cfg^.cfgClasses)
    liftIO $ print found
----
--    forM_ clss $ \c ->
--      unless (c `S.member` found)  (deleteClass c)
--    logProgress cfg "class-closure"

--    reduceInterfaces
--    logProgress cfg "reduce-interface"

  where
    handleFailedToLoad [] = return ()
    handleFailedToLoad errs = do
      hPutStrLn stderr "Could not load the following classes"
      mapM_ (\e -> hPutStr stderr "  - " >> hPutStrLn stderr (show e)) errs

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

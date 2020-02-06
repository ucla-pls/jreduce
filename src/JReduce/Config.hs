{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

module JReduce.Config where

-- lens
import           Control.Lens

-- mtl
import           Control.Monad.Reader

-- optparse-applicative
import           Options.Applicative                   as A

-- reduce-util
import           Control.Reduce.Util
import           Control.Reduce.Util.Logger            as L
import           Control.Reduce.Util.OptParse
import           Control.Reduce.Command

-- text
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

-- unordered-containers
import qualified Data.HashSet                          as HashSet

-- jvmhs
import           Jvmhs

data DumpConfig = DumpConfig
  { _cfgDumpGraph        :: !Bool
  , _cfgDumpClosures     :: !Bool
  , _cfgDumpLogic        :: !Bool
  , _cfgDumpCore         :: !Bool
  , _cfgDumpItems        :: !Bool
  } deriving (Show)

makeClassy ''DumpConfig

data Config = Config
  { _cfgLogger           :: !L.LoggerConfig
  , _cfgCore             :: !(HashSet.HashSet Text.Text)
  , _cfgClassPath        :: ![ FilePath ]
  , _cfgStubsFile        :: !(Maybe FilePath)
  , _cfgJreFolder        :: !(Maybe FilePath)
  , _cfgTarget           :: !FilePath
  , _cfgOutput           :: !(Maybe FilePath)
  , _cfgReducerName      :: !ReducerName
  , _cfgWorkFolder       :: !WorkFolder
  , _cfgDump             :: !DumpConfig
  , _cfgPredicateOptions :: !PredicateOptions
  , _cfgReductionOptions :: !ReductionOptions
  , _cfgCmdTemplate      :: !CmdTemplate
  } deriving (Show)

makeClassy ''Config

instance HasDumpConfig Config where
  dumpConfig = cfgDump

instance HasLogger Config where
  loggerL = cfgLogger

instance HasPredicateOptions Config where
  predicateOptions = cfgPredicateOptions

instance HasReductionOptions Config where
  reductionOptions = cfgReductionOptions

type MonadIOReader env m =
  (MonadIO m, MonadReader env m)

-- preloadClasses ::
--   (HasConfig env, HasLogger env, MonadIOReader env m)
--   => m ClassPreloader
-- preloadClasses = L.phase "Preloading Classes" $ do
--   cls <- createClassLoader
--   (classreader, numclasses) <- liftIO $ do
--     classreader <- preload cls
--     numclasses <- length <$> classes classreader
--     return (classreader, numclasses)
--   L.debug $ "Found" <-> display numclasses <-> "classes."
--   return classreader

fetchHierachy :: MonadIOReader Config m => [Class] -> m Hierarchy
fetchHierachy targets = L.phase "Calculating the hierarchy" $ do
  stubsfile <- view cfgStubsFile
  stdlib <- L.phase "Load stdlib stubs" $ do
    r <- view cfgJreFolder
      >>= liftIO . maybe (fromClassPath []) (fromJreFolder [])

    liftIO $ case stubsfile of
      Just fp ->
        computeStubsWithCache fp r
      Nothing ->
        computeStubs r

  stubs <- L.phase "Load project stubs" $ do
    (stubs, _) <- flip runClassPoolT mempty $ do
      cp <- view cfgClassPath
      errs <- loadClassesFromReader (ReaderOptions False (ClassLoader [] [] cp))
      forM errs $ L.warn . L.display
      forM_ targets putClass
      expandStubs stdlib
    return stubs

  hry <- L.phase "Compute hierarchy" $
    hierarchyFromStubsWarn
    (\a -> L.warn $ "Could not find: " <> toBuilder a)
    stubs

  return hry

parseDumpConfig :: Parser DumpConfig
parseDumpConfig = do
  _dump <- switch
    $ long "dump" <> hidden
    <> help "dump all to the workfolder."

  _dumpGraph <- switch
    $ long "dump-graph" <> hidden
    <> help "dump graph to the workfolder."

  _dumpClosures <- switch
    $ long "dump-closures" <> hidden
    <> help "dump closures to the workfolder."

  _dumpCore <- switch
    $ long "dump-core" <> hidden
    <> help "dump core to the workfolder."

  _dumpItems <- switch
    $ long "dump-items" <> hidden
    <> help "dump item terms to the workfolder."
  
  _dumpLogic <- switch
    $ long "dump-logic" <> hidden
    <> help "dump the logical statement to the workfolder."

  return $ DumpConfig
    { _cfgDumpGraph    = _dump || _dumpGraph
    , _cfgDumpClosures = _dump || _dumpClosures
    , _cfgDumpCore     = _dump || _dumpCore
    , _cfgDumpItems    = _dump || _dumpItems
    , _cfgDumpLogic    = _dump || _dumpLogic
    }
     
configParser :: Parser (IO Config)
configParser = do
  _cfgTarget <-
    strArgument
    $ metavar "INPUT"
    <> help "the path to the jar or folder to reduce."

  _cfgLogger <- parseLoggerConfig

  ioCore <-
    fmap readLines . many . strOption
    $ short 'c'
    <> long "core"
    <> metavar "CORE"
    <> hidden
    <> help "the core classes to not reduce. Can add a file of classes by prefixing @."

  _cfgClassPath <-
    fmap concat . many
    . fmap splitClassPath
    . strOption
    $ long "cp"
      <> hidden
      <> metavar "CLASSPATH"
      <> help ("the library classpath of things not reduced. "
               ++ "This is useful if the core files is not in the reduction, like when you are"
               ++ " reducing a library using a test-suite"
               )

  _cfgStubsFile <-
    optional . strOption $ long "stdlib"
    <> hidden
    <> help
    ( "load and save the stdlib to this stubsfile."
      <> " Choose between .json and .bin formats."
    )

  _cfgJreFolder <-
    optional . strOption $ long "jre"
    <> hidden
    <> metavar "JRE"
    <> help "the location of the stdlib."

  _cfgDump <-
    parseDumpConfig

  _cfgOutput <-
    parseOutputFile

  _cfgWorkFolder <-
    parseWorkFolder "jreduce"

  _cfgReducerName <-
    parseReducerName

  _cfgReductionOptions <-
    parseReductionOptions

  _cfgPredicateOptions <-
    parsePredicateOptions

  ioCmdTemplate <-
    parseCmdTemplate

  pure $ do
    _cfgCore <- ioCore
    _cfgCmdTemplate <- either fail return =<< ioCmdTemplate
    return $ Config {..}

  where
    readLines ls =
      fmap (HashSet.fromList . concat) . forM ls $ \case
        '@':filename -> Text.lines <$> Text.readFile filename
        cn           -> return [Text.pack cn]

{-# LANGUAGE DeriveFunctor       #-}
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


data Config = Config
  { _cfgLogger           :: !L.LoggerConfig
  , _cfgCore             :: !(HashSet.HashSet Text.Text)
  , _cfgClassPath        :: ![ FilePath ]
  , _cfgUseStdlib        :: !Bool
  , _cfgStubsFile        :: !(Maybe FilePath)
  , _cfgJreFolder        :: !(Maybe FilePath)
  , _cfgTarget           :: !FilePath
  , _cfgOutput           :: !(Maybe FilePath)
  , _cfgReducerName      :: !ReducerName
  , _cfgWorkFolder       :: !WorkFolder
  , _cfgPredicateOptions :: !PredicateOptions
  , _cfgReductionOptions :: !ReductionOptions
  , _cfgCmdTemplate      :: !CmdTemplate
  } deriving (Show)

makeClassy ''Config

instance HasLogger Config where
  loggerL = cfgLogger

instance HasPredicateOptions Config where
  predicateOptions = cfgPredicateOptions

instance HasReductionOptions Config where
  reductionOptions = cfgReductionOptions

type MonadIOReader env m = (MonadIO m, MonadReader env m)


preloadClasses ::
  (HasConfig env, HasLogger env, MonadIOReader env m)
  => m ClassPreloader
preloadClasses = L.phase "Preloading Classes" $ do
  cls <- createClassLoader
  (classreader, numclasses) <- liftIO $ do
    classreader <- preload cls
    numclasses <- length <$> classes classreader
    return (classreader, numclasses)
  L.debug $ "Found" <-> display numclasses <-> "classes."
  return classreader

-- | Create a class loader from the config
createClassLoader ::
  (HasConfig env, MonadIOReader env m)
  => m ClassLoader
createClassLoader = do
  cfg <- ask
  let cp = cfg ^. cfgTarget : cfg ^. cfgClassPath
  if cfg ^. cfgUseStdlib
    then
      case cfg ^. cfgJreFolder of
        Nothing ->
          liftIO $ fromClassPath cp
        Just jre ->
          liftIO $ fromJreFolder cp jre
    else
      return $ ClassLoader [] [] cp

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

  _cfgUseStdlib <-
    switch $ long "stdlib"
    <> hidden
    <> help "load the standard library?. This will take longer, but might be nesseary."

  _cfgStubsFile <-
    optional . strOption $ long "stubs"
    <> hidden
    <> help "save the stdlib to this stubsfile. Choose between .json and .bin formats."

  _cfgJreFolder <-
    optional . strOption $ long "jre"
    <> hidden
    <> metavar "JRE"
    <> help "the location of the stdlib."

  _cfgOutput <-
    parseOutputFile

  -- _cfgRecursive <-
  --   switch $
  --   long "recursive"
  --   <> short 'r'
  --   <> hidden
  --   <> help "remove other files and reduce internal jars."

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

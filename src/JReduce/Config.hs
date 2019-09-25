{-# LANGUAGE DeriveFunctor       #-}
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

-- unordered-containers
import qualified Data.HashSet                          as HashSet

-- base
import           Data.Char

-- jvmhs
import           Jvmhs

data Strategy
  = OverClasses
  | OverAll
  | OverStubs
  deriving (Ord, Eq, Show)

parseStrategy :: Parser Strategy
parseStrategy =
  option strategyReader
  $ short 'S'
  <> long "strategy"
  <> metavar "STRATEGY"
  <> hidden
  <> help ( "reduce by different granularity (default: all)." ++
            "Choose between class, stubs, and all." )
  <> value OverAll
  where
    strategyReader :: ReadM Strategy
    strategyReader = maybeReader $ \s ->
      case map toLower s of
        "class" -> Just OverClasses
        "all"  -> Just OverAll
        "stub" -> Just OverStubs
        _ -> Nothing

data Config = Config
  { _cfgLogger           :: !L.LoggerConfig
  , _cfgCore             :: !(HashSet.HashSet ClassName)
  , _cfgClassPath        :: ![ FilePath ]
  , _cfgUseStdlib        :: !Bool
  , _cfgJreFolder        :: !(Maybe FilePath)
  , _cfgTarget           :: !FilePath
  , _cfgOutput           :: !(Maybe FilePath)
  , _cfgStrategy         :: !Strategy
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


configParser :: Parser (IO Config)
configParser = do
  _cfgTarget <-
    strArgument
    $ metavar "INPUT"
    <> help "the path to the jar or folder to reduce."

  _cfgLogger <- parseLoggerConfig

  ioCore <-
    fmap readClassNames . many . strOption
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
    <> help "load the standard library?. This will take longer."

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

  _cfgStrategy <-
    parseStrategy

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
    readClassNames classnames' =
      fmap (HashSet.fromList . map strCls . concat) . forM classnames' $ \case
        '@':filename -> lines <$> readFile filename
        cn           -> return [cn]

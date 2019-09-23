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

module JReduce where

-- lens
import           Control.Lens

-- mtl
import           Control.Monad.Reader

-- filepath
import           System.FilePath

-- optparse-applicative
import           Options.Applicative                   as A

-- bytestring
import qualified Data.ByteString.Lazy                  as BL

-- reduce
import           Control.Reduce

-- casava
import qualified Data.Csv as C


-- reduce-util
import           Control.Reduce.Util
import           Control.Reduce.Util.Logger            as L
import           Control.Reduce.Util.OptParse
import           Control.Reduce.Metric
import           Control.Reduce.Command

-- dirtree
import           System.DirTree

-- unordered-containers
import qualified Data.HashSet                          as HashSet

-- containers
import qualified Data.IntSet                           as IS

-- base
import           Data.Maybe
import           Data.Monoid
import           Data.Char

-- jvmhs
import           Jvmhs

import JReduce.Target

import qualified JReduce.OverAll
import qualified JReduce.OverClasses
import qualified JReduce.OverStubs

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
    <> help "load the standard library? This is unnecessary for most reductions."

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

main :: IO ()
main = do
  cfg <- join . execParser $
    A.info (configParser <**> helper)
    ( fullDesc
    <> header "jreduce"
    <> progDesc "A command line tool for reducing java programs."
    )

  runReaderT run cfg

newtype DirTreeMetric = DirTreeMetric Int

dirTreeMetric :: Maybe (DirTree BL.ByteString) -> DirTreeMetric
dirTreeMetric =
  DirTreeMetric . fromIntegral . getSum . (foldMap . foldMap $ Sum . BL.length)

instance Metric DirTreeMetric where
  order = Const ["bytes"]
  fields (DirTreeMetric a) = [("bytes" C..= a)]
  displayMetric (DirTreeMetric a) =
    display (a `div` 1000) <> displayString " Kb"


run :: ReaderT Config IO ()
run = do
  Config {..} <- ask

  result <- withWorkFolder _cfgWorkFolder $ \wf -> do

    p0 <- orFail "Couldn't run problem in time"
      =<< setupProblemFromFile (wf </> "initial") _cfgCmdTemplate _cfgTarget

    let p1 = meassure dirTreeMetric p0

    p2 <- liftIO
          . refineProblemA (fmap (Just . undeepenTarget,) . deepenTarget)
          $ p1

    p3 <- p2 & case _cfgStrategy of
      OverAll -> JReduce.OverAll.describeProblem wf
      OverClasses -> pure . JReduce.OverClasses.describeProblem
      OverStubs -> pure . JReduce.OverStubs.describeProblem

    (_failure, result) <- runReductionProblem (wf </> "reduction")
      (genericBinaryReduction (IS.size . IS.unions))
      . meassure (Count "scc" . maybe 0 length)
      $ p3

    _ <- checkSolution (wf </> "final") p3 result

    return (fromJust $ _problemExtractBase p3 result)

  return ()
  -- Output the results
  -- outputResults result

  where
    outputResults target = do
      inputFile <- view cfgTarget
      possibleOutput <- view cfgOutput
      liftIO . flip (writeDirTree BL.writeFile) target
        =<< findOutputFile inputFile possibleOutput

    orFail msg = maybe (fail msg) return



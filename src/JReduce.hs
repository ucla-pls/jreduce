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

-- time 
import Data.Time.Clock

-- text
import qualified Data.Text as Text

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
import           Control.Reduce.Boolean.CNF
import           Control.Reduce.Util.Logger            as L
import           Control.Reduce.Util.OptParse
import           Control.Reduce.Metric

-- dirtree
import           System.DirTree

-- containers
-- import qualified Data.Set                              as S
import qualified Data.IntSet                           as IS

-- base
import           Data.Maybe
import           Data.Monoid
import           GHC.IO.Encoding (setLocaleEncoding, utf8)

-- jreduce
import JReduce.Target
import JReduce.Config
import qualified JReduce.Logic
import qualified JReduce.Classes

main :: IO ()
main = do
  setLocaleEncoding utf8

  (strat, getConfig) <- execParser $
    A.info (((,) <$> strategyParser <*> configParser) <**> helper)
    ( fullDesc
    <> header "jreduce"
    <> progDesc "A command line tool for reducing java programs."
    )

  cfg <- getConfig

  runReaderT (L.phase "JReduce" $ run strat) cfg

newtype DirTreeMetric = DirTreeMetric Int

dirTreeMetric :: Maybe (DirTree BL.ByteString) -> DirTreeMetric
dirTreeMetric =
  DirTreeMetric . fromIntegral . getSum . (foldMap . foldMap $ Sum . BL.length)

instance Metric DirTreeMetric where
  order = Const ["bytes"]
  fields (DirTreeMetric a) = [("bytes" C..= a)]
  displayMetric (DirTreeMetric a) =
    display (a `div` 1000) <> displayString " Kb"

run :: Strategy -> ReaderT Config IO ()
run strat = do
  Config {..} <- ask
  L.info "Started JReduce."
  start <- liftIO getCurrentTime

  result <- withWorkFolder _cfgWorkFolder $ \wf -> do

    p0 <- orFail "Couldn't run problem in time"
      =<< setupProblemFromFile (wf </> "initial") _cfgCmdTemplate _cfgTarget

    cfg <- view cfgLogicConfig
    let p1 = meassure dirTreeMetric p0
    case strat of
      OverClasses b -> do
        p2 <- targetProblem b $ p1
        p3 <- JReduce.Classes.describeProblem wf p2
        runBinary start wf 
          . meassure (Count "scc" . maybe 0 length)
          . set problemCost (fromIntegral . IS.size . IS.unions) 
          $ p3

      OverLogicGraph -> do
        p2 <- targetProblem True $ p1
        p3 <- JReduce.Logic.describeGraphProblem cfg wf p2
        runBinary start wf 
          . meassure (Count "scc" . maybe 0 length)
          . set problemCost (fromIntegral . IS.size . IS.unions) 
          $ p3
      
      OverLogicApprox -> do
        p2 <- targetProblem True $ p1
        (ipf, p3) <- JReduce.Logic.describeLogicProblem cfg wf p2
        runBinary start wf 
          . JReduce.Logic.approxLogicProblem ipf
          . meassure (Count "vars" . maybe 0 IS.size)
          . set problemCost (fromIntegral . IS.size) 
          $ p3

      OverLogic -> do
        p2 <- targetProblem True $ p1
        (ipf, p3) <- JReduce.Logic.describeLogicProblem cfg wf p2
        (failure, result) <- runReductionProblem start (wf </> "reduction")
          (ipfBinaryReduction ipf)
          . meassure (Count "vars" . maybe 0 IS.size)
          . set problemCost (fromIntegral . IS.size)
          $ p3
        checkResults wf p3 (failure, result)

  -- Output the results
  outputResults result

  where
    checkResults wf p3 (failure, result) = do
      case failure of
        Just msg ->
          L.warn $ "Reduction failed: " <> display msg
        Nothing ->
          L.info $ "Reduction successfull."

      local (redKeepFolders .~ True) $
        void $ checkSolution (wf </> "final") p3 result

      return (fromJust $ _problemExtractBase p3 result)
      
    runBinary start wf p3 = do
      (failure, result) <- runReductionProblem start (wf </> "reduction")
        genericBinaryReduction
        $ p3
      checkResults wf p3 (failure, result)


    outputResults target = do
      inputFile <- view cfgTarget
      possibleOutput <- view cfgOutput
      liftIO . flip (writeDirTree BL.writeFile) target
        =<< findOutputFile inputFile possibleOutput

    orFail msg = maybe (fail msg) return

data Strategy
  = OverClasses Bool
  | OverLogicGraph 
  | OverLogicApprox 
  | OverLogic 
  deriving (Show)

strategyParser :: Parser Strategy
strategyParser =
  option strategyReader
  $ short 'S'
  <> long "strategy"
  <> metavar "STRATEGY"
  <> hidden
  <> help
    ( "reduce by different granularity (default: logic)."
      ++ "Choose between classes, logic, and logic+graph."
    )
  <> value OverLogic
  where
    strategyReader :: ReadM Strategy
    strategyReader = maybeReader $ \s ->
      case Text.toLower . Text.pack $ s of
        "classes" -> Just $ OverClasses True
        "classes+flat" -> Just $ OverClasses False
        "logic+graph" ->
          Just $ OverLogicGraph 
        "logic+approx" ->
          Just $ OverLogicApprox 
        "logic" ->
          Just $ OverLogic 
        _ -> Nothing

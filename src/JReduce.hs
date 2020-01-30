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
import           Control.Reduce.Util.Logger            as L
import           Control.Reduce.Util.OptParse
import           Control.Reduce.Metric

-- dirtree
import           System.DirTree

-- containers
import qualified Data.IntSet                           as IS

-- base
import           Data.Maybe
import           Data.Monoid
import           GHC.IO.Encoding (setLocaleEncoding, utf8)

-- jreduce
import JReduce.Target
import JReduce.Config
import qualified JReduce.OverDeep
import qualified JReduce.OverLogic
import JReduce.OverDeep (EdgeSelection (..))
import qualified JReduce.OverClasses
-- import qualified JReduce.OverStubs

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

  result <- withWorkFolder _cfgWorkFolder $ \wf -> do

    p0 <- orFail "Couldn't run problem in time"
      =<< setupProblemFromFile (wf </> "initial") _cfgCmdTemplate _cfgTarget

    let p1 = meassure dirTreeMetric p0

    p2 <- targetProblem $ p1

    p3 <- p2 & case strat of
      OverDeep selection ->
        JReduce.OverDeep.describeProblem selection wf
      OverClasses ->
        JReduce.OverClasses.describeProblem wf
      OverLogicApprox ext bool ->
        JReduce.OverLogic.describeGraphProblem ext bool wf
      OverLogic -> error "Not supported, yet!"

    (failure, result) <- runReductionProblem (wf </> "reduction")
      (genericBinaryReduction (IS.size . IS.unions))
      . meassure (Count "scc" . maybe 0 length)
      $ p3

    case failure of
      Just msg ->
        L.warn $ "Reduction failed: " <> display msg
      Nothing ->
        L.info $ "Reduction successfull."

    local (redKeepFolders .~ True) $
      void $ checkSolution (wf </> "final") p3 result


    return (fromJust $ _problemExtractBase p3 result)

  -- Output the results
  outputResults result

  where
    outputResults target = do
      inputFile <- view cfgTarget
      possibleOutput <- view cfgOutput
      liftIO . flip (writeDirTree BL.writeFile) target
        =<< findOutputFile inputFile possibleOutput

    orFail msg = maybe (fail msg) return

data Strategy
  = OverClasses
  | OverDeep EdgeSelection
  | OverLogicApprox Bool Bool
  | OverLogic
  deriving (Ord, Eq, Show)

strategyParser :: Parser Strategy
strategyParser =
  option strategyReader
  $ short 'S'
  <> long "strategy"
  <> metavar "STRATEGY"
  <> hidden
  <> help
    ( "reduce by different granularity (default: deep)."
      ++ "Choose between class, stubs, and deep."
    )
  <> value OverClasses
  where
    strategyReader :: ReadM Strategy
    strategyReader = maybeReader $ \s ->
      case Text.split (=='+') . Text.toLower . Text.pack $ s of
        "classes":[] -> Just OverClasses
        "deep":rest -> Just $ OverDeep (foldMap toEdgeSelection rest)
        ["logic", "under"] ->
          Just $ OverLogicApprox False False
        ["logic", "over"] ->
          Just $ OverLogicApprox False True
        ["logic", "extends", "over"] ->
          Just $ OverLogicApprox True True
        ["logic"] ->
          Just $ OverLogic
        _ -> Nothing

      where
        toEdgeSelection = \case
          "m2m" ->  mempty { edgeSelectMethodsToMethods = True }
          "i2m" -> mempty { edgeSelectInterfacesToMethods = True }
          "nostub" -> mempty { edgeSelectMethodsToCode = True }
          _ -> mempty

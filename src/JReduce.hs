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

-- dirtree
import           System.DirTree

-- containers
import qualified Data.IntSet                           as IS

-- base
import           Data.Maybe
import           Data.Monoid

-- jvmhs
import           Jvmhs

-- jreduce
import JReduce.Target
import JReduce.Config
import qualified JReduce.OverAll
import qualified JReduce.OverClasses
import qualified JReduce.OverStubs

main :: IO ()
main = do
  cfg <- join . execParser $
    A.info (configParser <**> helper)
    ( fullDesc
    <> header "jreduce"
    <> progDesc "A command line tool for reducing java programs."
    )

  runReaderT (L.phase "JReduce" $ run) cfg

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

    p2 <- targetProblem $ p1

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

  -- Output the results
  outputResults result

  where
    outputResults target = do
      inputFile <- view cfgTarget
      possibleOutput <- view cfgOutput
      liftIO . flip (writeDirTree BL.writeFile) target
        =<< findOutputFile inputFile possibleOutput

    orFail msg = maybe (fail msg) return

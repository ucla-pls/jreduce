{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}

module JReduce.Target
  ( Target 
  , Content (..)
  , _ClassFile
  , _Jar
  , _MetaData

  , targetProblem
  , displayTarget
  )
where

-- lens
import           Control.Lens

-- -- deepseq
-- import           Control.DeepSeq



-- cassava
import           Data.Csv as C

-- zip-archive
import           Codec.Archive.Zip

-- bytestring
import qualified Data.ByteString.Lazy                  as BL

-- dirtree
import           System.DirTree
import           System.DirTree.Zip

-- nfdata
import Control.DeepSeq

-- base
import           Control.Exception
import           GHC.Generics (Generic)
import           Data.Maybe
import           Control.Monad.IO.Class

-- reduce
import           Control.Reduce.Problem
import           Control.Reduce.Metric
import           Control.Reduce.Util.Logger
import           Control.Reduce.Reduction

-- jvhms
import           Jvmhs

-- jreduce
import JReduce.Config

-- | The content of a file is either a Class, a Jar, or some MetaData.
data Content
  = ClassFile Class
  | Jar (DirForest Content)
  | MetaData !BL.ByteString
  deriving (Show, Eq, Generic)

makePrisms ''Content

instance NFData Content

-- | We define the target of the reduction as
type Target = DirTree Content

data TargetMetric = TargetMetric
  { _targetMetricClasses  :: !Int
  , _targetMetricJars        :: !Int
  , _targetMetricOtherFiles :: !Int
  }

makeLenses ''TargetMetric

instance Semigroup TargetMetric where
  TargetMetric a1 b1 c1
    <> TargetMetric a2 b2 c2
    = TargetMetric (a1 + a2) (b1 + b2) (c1 + c2)

instance Monoid TargetMetric where
  mempty = TargetMetric 0 0 0

targetMetric :: Maybe Target -> TargetMetric
targetMetric =
  view (folded.folded.deepSubelements contentR.to metric)
  where
    metric :: Content -> TargetMetric
    metric = \case
      Jar _ -> mempty & targetMetricJars .~ 1
      ClassFile _ -> mempty & targetMetricClasses .~ 1
      MetaData _ -> mempty & targetMetricOtherFiles .~ 1

contentR :: PartialReduction Content Content
contentR f = \case
  Jar c -> fmap Jar <$> deepDirForestR f c
  a -> pure $ Just a


targetProblem :: MonadIOReader env m
  => Problem a (DirTree BL.ByteString) -> m (Problem a Target)
targetProblem p1 = do
    p2 <- liftIO $ refineProblemA (fmap (Just . undeepenTarget,) . deepenTarget) p1
    return $ meassure targetMetric p2


instance Metric TargetMetric where
  order = Const ["jars", "classes", "other-files"]
  fields (TargetMetric {..}) =
    ["classes" C..= _targetMetricClasses
    , "jars" C..= _targetMetricJars
    , "other-files" C..= _targetMetricOtherFiles
    ]
  displayMetric (TargetMetric {..}) =
    display _targetMetricClasses
    <-> displayString "classes,"
    <-> display _targetMetricJars
    <-> displayString "jars,"
    <-> display _targetMetricOtherFiles
    <-> displayString "other"


deepenTarget :: DirTree BL.ByteString -> IO Target
deepenTarget = imapM (readTargetFile . fileKeyToPath) where
  readTargetFile :: FilePath -> BL.ByteString -> IO Content
  readTargetFile fp bst =
    case asClassName fp of
      Just _ ->
        return $ ClassFile (either (error . show) fromClassFile $ readClassFile' True bst)
      Nothing
        | isJar fp -> handle (\(SomeException _) -> return $ MetaData bst) $ do
            d1 <- maybe (fail "Jar contains external links") return
              <$> followInternalLinks . directory $ toArchive bst ^. files
            d2 <- imapM (readTargetFile . fileKeyToPath) d1
            return . Jar $ d2 ^?! _Directory
        | otherwise -> return $ MetaData bst

undeepenTarget :: Target -> DirTree BL.ByteString
undeepenTarget = fmap writeTargetFile where
    writeTargetFile :: Content -> BL.ByteString
    writeTargetFile = \case
      ClassFile cl -> serializeClass cl
      MetaData b -> b
      Jar f ->
        fromArchive
        . (files .~ (Real . writeTargetFile <$> f))
        $ emptyArchive

followInternalLinks :: RelativeDirTree Link a -> Maybe (DirTree a)
followInternalLinks tree =
  let
    x = flip flattenDirTree tree $ \case
      File (Real a) -> Just (file a)
      File (Symlink (External _)) -> Nothing
      File (Symlink (Internal f)) -> x ^? _Just . ix f
      Directory fm ->
        Just
        . directory' . mapMaybe (\(i, m) -> (i,) <$> m) . itoList
        $ fm
  in x

displayTarget :: Target -> IO ()
displayTarget = go "" where
  go prefix =
    ifoldMap $ \fk a -> do
      putStr prefix >> putStr (show fk) >> putStr "  ->  "
      case a of
        MetaData _ -> putStrLn "[data]"
        Jar t -> putStrLn "[jar]" >> go (prefix ++ "> ") (directory t)
        ClassFile cls -> print (cls ^. className)

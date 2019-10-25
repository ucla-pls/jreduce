{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module JReduce.Target
  ( Target
  , Content (..)
  , _ClassFile
  , _Jar
  , _MetaData

  , targetProblem
  , targetClasses
  , fetchHierachy
  , displayTarget
  , describeProblemTemplate
  )
where

-- lens
import          Control.Lens

-- containers
import qualified Data.IntSet                as IS

-- unorderd-containers
import qualified Data.HashMap.Strict        as HM
import qualified Data.HashSet               as HS

-- cassava
import           Data.Csv                   as C

-- zip-archive
import           Codec.Archive.Zip

-- bytestring
import qualified Data.ByteString.Lazy       as BL

-- dirtree
import           System.DirTree
import           System.DirTree.Zip

-- vector
import qualified Data.Vector as V

-- filepath
import System.FilePath

-- directory
import System.Directory

-- text
import qualified Data.Text as Text
import Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Data.Text.Lazy.IO as LazyText

-- nfdata
import           Control.DeepSeq

-- base
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.List                  as List
import           Data.Maybe
import           Data.Coerce
import           Data.Bifunctor
import           Data.Monoid
import           Text.Printf
import           GHC.Generics               (Generic)

-- reduce
import           Control.Reduce.Metric
import           Control.Reduce.Problem
import           Control.Reduce.Graph
import           Control.Reduce.Reduction
import           Control.Reduce.Util.Logger as L

-- jvhms
import           Jvmhs

-- jreduce
import           JReduce.Config

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
  { _targetMetricClasses    :: !Int
  , _targetMetricJars       :: !Int
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

fetchHierachy :: MonadIOReader Config m => [Class] -> m Hierarchy
fetchHierachy targets = L.phase "Calculating the hierachy" $ do
  r <- preloadClasses

  -- TODO Fix read order so that classes that override the classpath
  -- is loaded first.
  hry <- fmap (snd . fst) . flip runClassPoolT mempty
    $ do
    L.phase "Loading classes in class path" .  void
      $ loadClassesFromReader (ReaderOptions False r)

    forM_ targets putClass
     
    getHierarchy

  L.debug $ "Hierachy calculated, processed #"
    <> L.display (HM.size $ hry ^. hryStubs)
    <> " classes."

  return hry


describeProblemTemplate ::
  (MonadIOReader Config m)
  => PartialReduction i i
  -> m (i -> (k, [k]))
  -> (k -> Builder)
  -> Prism' i Target
  -> FilePath
  -> Problem a Target
  -> m (Problem a [IS.IntSet])
describeProblemTemplate itemR genKeyFun displayK _ITarget wf p = do
  let
    p2 = liftProblem (review _ITarget) (fromJust . preview _ITarget) p

  keyFun <- L.phase "Initializing key function" $ genKeyFun

  L.phase "Precalculating the Reduction" $ do
    core <- view cfgCore
    L.info . L.displayf "Requiring %d core items." $ List.length core

    ((grph, coreSet, cls), p3) <- toGraphReductionDeepM
      ( \i ->
          let (k, items) = keyFun i
              txt = serializeWith displayK k
              isCore = txt `HS.member` core
          in L.logtime L.DEBUG ("Processing " <> displayK k <> (if isCore then " CORE" else ""))  $
            pure ( txt
                 , isCore
                 , map (serializeWith displayK) items
                 )
      ) itemR p2

    L.info . L.displayf "Found Core: %d"
      $ IS.size coreSet

    liftIO $ do
      LazyText.writeFile (wf </> "core.txt")
        . Builder.toLazyText
        $ foldMap
          (\x -> (displayGraphField . GraphField $ nodeLabels grph V.! x) <> "\n")
          (IS.toList coreSet)

    L.info . L.displayf "Found Number of Closures: %d"
      $ List.length cls

    liftIO $ do
      createDirectory (wf </> "closures")
      iforM_ cls $ \i c -> do
        LazyText.writeFile (wf </> "closures" </> printf "%05d.txt" i)
          . Builder.toLazyText
          $ foldMap
            (\x -> (displayGraphField . GraphField $ nodeLabels grph V.! x) <> "\n")
            (IS.toList c)

    L.phase "Outputing graph: " $ do
      liftIO . BL.writeFile (wf </> "graph.csv")
        . writeCSV
        $ ( coerce . first (const ("" :: Text.Text))
           $ grph :: Control.Reduce.Graph.Graph Text.Text GraphField
          )


    return p3

targetClasses :: Target -> [Class]
targetClasses = toListOf (folded.go)
  where
    go :: Getting (Endo [Class]) Content Class
    go = _ClassFile <> _Jar.folded.go

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


newtype GraphField = GraphField ([Int], Text.Text)

displayGraphField :: GraphField -> Builder
displayGraphField (GraphField (i, a)) =
  "|" <> fromString (List.intercalate "|". map show . reverse $ i)
  <> " " <> Builder.fromText a

instance ToField GraphField where
  toField gf =
    BL.toStrict
    . LazyText.encodeUtf8
    . Builder.toLazyText
    $ displayGraphField gf

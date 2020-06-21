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
  , Content(..)
  , _ClassFile
  , _Jar
  , _MetaData
  , targetProblem
  , targetClasses
  , fetchHierachy
  , displayTarget
  , describeProblemTemplate
  , dumpGraphInfo
  )
where

-- lens
import           Control.Lens

-- containers
import qualified Data.IntSet                   as IS

-- unorderd-containers
import qualified Data.HashSet                  as HS

-- cassava
import           Data.Csv                      as C

-- zip-archive
import           Codec.Archive.Zip

-- bytestring
import qualified Data.ByteString.Lazy          as BL

-- dirtree
import           System.DirTree
import           System.DirTree.Zip

-- vector
import qualified Data.Vector                   as V

-- filepath
import           System.FilePath

-- directory
import           System.Directory

-- text
import qualified Data.Text                     as Text
import           Data.Text.Lazy.Builder        as Builder
import qualified Data.Text.Lazy.Encoding       as LazyText
import qualified Data.Text.Lazy.IO             as LazyText
import qualified Data.Text.Lazy                as LazyText

-- nfdata
import           Control.DeepSeq

-- base
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.List                     as List
import           Data.Maybe
import           Data.Bifunctor
import           Data.Monoid
import           Text.Printf
import           GHC.Generics                   ( Generic )

-- reduce
import           Control.Reduce.Metric
import           Control.Reduce.Problem
import           Control.Reduce.Graph
import           Control.Reduce.Reduction
import           Control.Reduce.Util.Logger    as L

-- jvhms
import           Jvmhs
import           Jvmhs.Format.ClassFile

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
  TargetMetric a1 b1 c1 <> TargetMetric a2 b2 c2 =
    TargetMetric (a1 + a2) (b1 + b2) (c1 + c2)

instance Monoid TargetMetric where
  mempty = TargetMetric 0 0 0

targetMetric :: Maybe Target -> TargetMetric
targetMetric = view (folded . folded . deepSubelements contentR . to metric)
 where
  metric :: Content -> TargetMetric
  metric = \case
    Jar       _ -> mempty & targetMetricJars .~ 1
    ClassFile _ -> mempty & targetMetricClasses .~ 1
    MetaData  _ -> mempty & targetMetricOtherFiles .~ 1

contentR :: PartialReduction Content Content
contentR f = \case
  Jar c -> fmap Jar <$> deepDirForestR f c
  a     -> pure $ Just a

targetProblem
  :: MonadIOReader env m
  => Bool
  -> Problem a (DirTree BL.ByteString)
  -> m (Problem a Target)
targetProblem expandJar p1 = do
  p2 <- liftIO
    $ refineProblemA (fmap (Just . undeepenTarget, )
      . deepenTarget expandJar) p1
  return $ meassure targetMetric p2


describeProblemTemplate
  :: (MonadIOReader Config m)
  => PartialReduction i i
  -> m (i -> m (k, [(k, k)]))
  -> (k -> Builder)
  -> Prism' i Target
  -> FilePath
  -> Problem a Target
  -> m (Problem a [IS.IntSet])
describeProblemTemplate itemR genKeyFun displayK _ITarget wf p = do
  let p2 = liftProblem (review _ITarget) (fromJust . preview _ITarget) p

  keyFun <- L.phase "Initializing key function" $ genKeyFun

  L.phase "Precalculating the Reduction" $ do
    core <- view cfgCore
    L.info . L.displayf "Requiring %d core items." $ List.length core

    ((grph, coreSet, cls), p3) <- toGraphReductionDeepM
      (\i -> do
        (k, items) <- keyFun i
        let txt    = serializeWith displayK k
            isCore = txt `HS.member` core

        a <-
          L.logtime
              L.DEBUG
              ("Processing " <> displayK k <> (if isCore then " CORE" else ""))
            $ let a = map (over both $ serializeWith displayK) items
              in  deepseq a (pure a)
        L.debug $ L.displayf "Found %d edges." (length a)
        return (txt, isCore, a)
      )
      itemR
      p2

    dumpGraphInfo wf (grph <&> LazyText.unpack . Builder.toLazyText . displayGraphField . GraphField) coreSet cls
    return p3

dumpGraphInfo
  :: (MonadIOReader Config m)
  => FilePath
  -> Control.Reduce.Graph.Graph b String
  -> IS.IntSet
  -> [IS.IntSet]
  -> m ()
dumpGraphInfo wf grph coreSet cls = do
  L.info . L.displayf "Found Core: %d" $ IS.size coreSet

  L.info . L.displayf "Found Number of Closures: %d" $ List.length cls

  whenM (view cfgDumpCore) $ do
    L.phase "Outputing core: " . liftIO $ do
      LazyText.writeFile (wf </> "core.txt") . Builder.toLazyText $ foldMap
        (\x -> (displayString $ nodeLabels grph V.! x) <> "\n")
        (IS.toList coreSet)

  whenM (view cfgDumpClosures) $ do
    L.phase "Outputing closures: " . liftIO $ do
      createDirectory (wf </> "closures")
      iforM_ cls $ \i c -> do
        LazyText.writeFile (wf </> "closures" </> printf "%05d.txt" i)
          . Builder.toLazyText
          $ foldMap
              (\x ->
                (displayString $ nodeLabels grph V.! x) <> "\n"
              )
              (IS.toList c)

  whenM (view cfgDumpGraph) $ do
    L.phase "Outputing graph: " . liftIO $ do
      BL.writeFile (wf </> "graph.csv")
        . writeCSV
        $ first (const ([] :: String)) grph

targetClasses :: Target -> [Class]
targetClasses = toListOf (folded . go)
 where
  go :: Getting (Endo [Class]) Content Class
  go = _ClassFile <> _Jar . folded . go

instance Metric TargetMetric where
  order = Const ["jars", "classes", "other-files"]
  fields (TargetMetric {..}) =
    [ "classes" C..= _targetMetricClasses
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

deepenTarget :: Bool -> DirTree BL.ByteString -> IO Target
deepenTarget expandJar = imapM (readTargetFile . fileKeyToPath) where
  readTargetFile :: FilePath -> BL.ByteString -> IO Content
  readTargetFile fp bst = case asClassName fp of
    Just _ -> return . either (const (MetaData bst)) (ClassFile . removeOverrideAnnotations) $ do
        cf <- first show (readClassFile' True bst)
        first unlines $ fromClassFile cf

    Nothing
      | isJar fp && expandJar ->
        handle (\(SomeException _) -> return $ MetaData bst) $ do
          d1 <-
            maybe (fail "Jar contains external links") return
            <$> followInternalLinks
            .   directory
            $   toArchive bst
            ^.  files
          d2 <- imapM (readTargetFile . fileKeyToPath) d1
          return . Jar $ d2 ^?! _Directory
      | otherwise -> return $ MetaData bst

  -- TODO: Maybe model this instead
  removeOverrideAnnotations =
    classMethods . traversed . methodAnnotations %~
    filter (notElemOf annotationType "java/lang/Override")


undeepenTarget :: Target -> DirTree BL.ByteString
undeepenTarget = fmap writeTargetFile where
  writeTargetFile :: Content -> BL.ByteString
  writeTargetFile = \case
    ClassFile cl -> serializeClass cl
    MetaData  b  -> b
    Jar f ->
      fromArchive . (files .~ (Real . writeTargetFile <$> f)) $ emptyArchive

followInternalLinks :: RelativeDirTree Link a -> Maybe (DirTree a)
followInternalLinks tree =
  let x = flip flattenDirTree tree $ \case
        File (Real    a           ) -> Just (file a)
        File (Symlink (External _)) -> Nothing
        File (Symlink (Internal f)) -> x ^? _Just . ix f
        Directory fm ->
          Just . directory' . mapMaybe (\(i, m) -> (i, ) <$> m) . itoList $ fm
  in  x

displayTarget :: Target -> IO ()
displayTarget = go "" where
  go prefix = ifoldMap $ \fk a -> do
    putStr prefix >> putStr (show fk) >> putStr "  ->  "
    case a of
      MetaData  _   -> putStrLn "[data]"
      Jar       t   -> putStrLn "[jar]" >> go (prefix ++ "> ") (directory t)
      ClassFile cls -> print (cls ^. className)


newtype GraphField = GraphField ([Int], Text.Text)

displayGraphField :: GraphField -> Builder
displayGraphField (GraphField (i, a)) =
  "|"
    <> fromString (List.intercalate "|" . map show . reverse $ i)
    <> " "
    <> Builder.fromText a

instance ToField GraphField where
  toField gf =
    BL.toStrict . LazyText.encodeUtf8 . Builder.toLazyText $ displayGraphField
      gf

whenM :: Monad m => (m Bool) -> m () -> m ()
whenM mb m = mb >>= \b -> when b m

-- unlessM :: Monad m => (m Bool) -> m () -> m ()
-- unlessM mb m = mb >>= \b -> when b m

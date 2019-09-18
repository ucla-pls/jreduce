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

-- -- deepseq
-- import           Control.DeepSeq

-- zip-archive
import           Codec.Archive.Zip

-- mtl
import           Control.Monad.Reader

-- void
import Data.Void

-- filepath
import           System.FilePath

-- cassava
import qualified Data.Csv                              as C

-- optparse-applicative
import           Options.Applicative                   as A

-- bytestring
import qualified Data.ByteString.Lazy                  as BL
import qualified Data.ByteString                       as BS

-- reduce
import           Control.Reduce

-- reduce-util
import           Control.Reduce.Util
import           Control.Reduce.Reduction
import           Control.Reduce.Util.Logger            as L hiding (update,
                                                             view)
import           Control.Reduce.Util.OptParse
import           Control.Reduce.Metric
import           Control.Reduce.Command

-- dirtree
import           System.DirTree
import           System.DirTree.Zip

-- unordered-containers
import qualified Data.HashMap.Strict                   as HashMap
import qualified Data.HashSet                          as HashSet

-- containers
import qualified Data.IntSet                           as IS
import qualified Data.Set                              as S
import qualified Data.Map                              as Map

-- unliftio
import           UnliftIO.Directory

-- text
import qualified Data.Text.Lazy.Builder as Builder

-- time
import Data.Time.Clock.System

-- nfdata
import Control.DeepSeq

-- base
import           Data.Foldable
import           Control.Exception
import           Text.Printf
import           GHC.Generics (Generic)
import           Data.Functor
import           Data.Char
import qualified Data.List as L
import           Data.Maybe
import           System.Exit

-- jvhms
import           Jvmhs
import           Jvmhs.Transform.Stub

-- | The content of a file is either a Class, a Jar, or some MetaData.
data Content
  = ClassFile Class
  | Jar (DirForest Content)
  | MetaData !BL.ByteString
  deriving (Show, Eq, Generic)

instance NFData Content

-- | We define the target of the reduction as
type Target = DirTree Content

data Item
  = IContent Content
  | ICode Code
  | ITarget Target

makePrisms ''Item

data Key
  = KClassName ClassName
  deriving (Eq, Ord)

keyFun :: Item -> (Maybe Key, [Key])
keyFun = \case
  IContent (ClassFile cls) ->
    ( Just (KClassName $ cls ^.className)
    , map KClassName
      . toListOf
      ( classSuper . _Just
        <> classInterfaces . traverse
        <> classFields . traverse . classNames
        <> classMethods . traverse . methodClassNames
        <> classBootstrapMethods . traverse . classNames
        <> classEnclosingMethod . _Just . (_1 <> _2 . _Just . classNames)
        <> classInnerClasses . traverse . classNames
      )
      $ cls
    )
    where
      methodClassNames =
        methodDescriptor . classNames
        <> methodExceptions . traverse
        <> methodSignature . _Just . classNames

  ICode code ->
    ( Nothing
    , code ^.. classNames . to KClassName
    )
  a -> (Nothing, [])

itemR :: PartialReduction Item Item
itemR f = \case
  ITarget t ->
    fmap ITarget <$> targetR f t
  IContent c ->
    fmap IContent <$> contentR f c
  a -> pure (Just a)
  where
    contentR :: PartialReduction Content Item
    contentR f = \case
      ClassFile c -> fmap ClassFile <$> (part classR) f c
      Jar c       -> fmap Jar <$> (deepDirForestR . reduceAs _IContent) f c
      a           -> pure (Just a)

    targetR :: PartialReduction Target Item
    targetR = deepDirTreeR . reduceAs _IContent

classR :: Reduction Class Item
classR = classMethods . traverse . methodR

methodR :: Reduction Method Item
methodR f m =
  case m ^. methodCode of
    Just c -> f (ICode c) <&> \case
      Just (ICode c') -> m & methodCode ?~ c'
      Nothing
        | m ^. methodId /= "<clinit>" && m ^. methodId /= "<init>" -> stub m
        | otherwise -> m
    z -> pure m





data Config = Config
  { _cfgLogger           :: !L.LoggerConfig
  , _cfgCore             :: !(HashSet.HashSet ClassName)
  , _cfgClassPath        :: ![ FilePath ]
  , _cfgUseStdlib        :: !Bool
  , _cfgJreFolder        :: !(Maybe FilePath)
  , _cfgTarget           :: !FilePath
  , _cfgOutput           :: !(Maybe FilePath)
  -- , _cfgRecursive        :: !Bool
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

  -- _cfgStrategy <-
  --   option strategyReader
  --   $ short 'S'
  --   <> long "strategy"
  --   <> metavar "STRATEGY"
  --   <> hidden
  --   <> help ( "reduce by class instead of by closure (default: closure)." ++
  --             "Choose between closure, reject-item, and item." )
  --   <> value ByClosure

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

run :: ReaderT Config IO ()
run = do
  Config {..} <- ask

  result <- withWorkFolder _cfgWorkFolder $ \wf -> do

    problem <- orFail "Couldn't run problem in time"
      =<< setupProblemFromFile (wf </> "initial") _cfgCmdTemplate _cfgTarget

    p1 <- liftIO . flip refineProblemA problem $ \s ->
      ((\case
           ITarget t -> Just $ undeepenTarget t
           a -> Nothing
       ,) . ITarget) <$> deepenTarget s

    let p2 =
          meassure (Count "scc" . maybe 0 length)
          . toGraphReductionDeep keyFun itemR
          $ p1

    (failure, result) <- runReductionProblem (wf </> "reduction")
      (genericBinaryReduction (IS.size . IS.unions))
      p2

    return (fromJust $ _problemExtractBase p2 result)

  -- Output the results
  outputResults result

  where
    outputResults target = do
      inputFile <- view cfgTarget
      possibleOutput <- view cfgOutput
      liftIO . flip (writeDirTree BL.writeFile) target
        =<< findOutputFile inputFile possibleOutput

    orFail msg = maybe (fail msg) return


deepenTarget :: DirTree BL.ByteString -> IO Target
deepenTarget = imapM (readTargetFile . fileKeyToPath) where
  readTargetFile :: FilePath -> BL.ByteString -> IO Content
  readTargetFile fp bst =
    case asClassName fp of
      Just _ ->
        return $ ClassFile (either (error . show) fromClassFile $ readClassFile' True bst)
      Nothing
        | isJar fp -> handle (\(SomeException _) -> return $ MetaData bst) $ do
            let Just d1 = followInternalLinks . directory $ toArchive bst ^. files
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
      File (Symlink (External f)) -> Nothing
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
        MetaData a -> putStrLn "[data]"
        Jar t -> putStrLn "[jar]" >> go (prefix ++ "> ") (directory t)
        ClassFile cls -> print (cls ^. className)


-- | Create a class loader from the config
createClassLoader ::
  (HasConfig env, MonadReader env m, MonadIO m)
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

-- data ClassCounter a = ClassCounter
--   { ccClosures :: Int
--   , ccClasses  :: Int
--   , ccContent  :: a
--   } deriving (Functor)

-- fromClasses :: Foldable t => t a -> ClassCounter (t a)
-- fromClasses classData =
--   ClassCounter 0 (length classData) classData

-- instance Metric (ClassCounter a) where
--   order = Const ["scc", "classes"]
--   fields ClassCounter {..} =
--     [ "scc" C..= ccClosures
--     , "classes" C..= ccClasses
--     ]

-- data Validation
--   = Reject
--   | Shrink
--   | NoValidation
--   deriving (Show, Eq)

-- data Strategy
--   = ByItem Validation
--   | ByClosure
--   deriving (Show, Eq)

-- strategyReader :: ReadM Strategy
-- strategyReader = maybeReader $ \s ->
--   asum
--     [ check "closure" $> ByClosure
--     , check "reject-item" $> ByItem Reject
--     , check "shrink-item" $> ByItem Shrink
--     , check "item" $> ByItem NoValidation
--     ]
--   where check = guard . L.isPrefixOf (map toLower s)

-- data Config = Config
--   { _cfgLogger           :: !L.LoggerConfig
--   , _cfgCore             :: !(HashSet.HashSet ClassName)
--   , _cfgClassPath        :: ![ FilePath ]
--   , _cfgUseStdlib        :: !Bool
--   , _cfgJreFolder        :: !(Maybe FilePath)
--   , _cfgTarget           :: !FilePath
--   , _cfgOutput           :: !(Maybe FilePath)
--   , _cfgRecursive        :: !Bool
--   , _cfgStrategy         :: !Strategy
--   , _cfgReducerName      :: !ReducerName
--   , _cfgPredicateOptions :: !PredicateOptions
--   , _cfgReductionOptions :: !ReductionOptions
--   , _cfgCmdTemplates     :: !CmdTemplate
--   } deriving (Show)



-- main :: IO ()
-- main = do
--   cfg <- join . execParser $
--     A.info (configParser <**> helper)
--     ( fullDesc
--     <> header "jreduce"
--     <> progDesc "A command line tool for reducing java programs."
--     )

--   runReaderT run cfg

-- run :: ReaderT Config IO ()
-- run = do
--   workFolder <- view $ cfgPredicateOptions . to predOptWorkFolder
--   (tclss, textra') <- unpackTarget (workFolder </> "unpacked")
--   let clss = toClassList tclss

--   textra <- view cfgRecursive >>= \case
--     True -> L.phase "remove extra files" $ do
--       let extras = toFileList textra'
--       predOpt' <- view cfgPredicateOptions
--       let predOpt = predOpt' { predOptWorkFolder = workFolder </> "extra-files" }
--       toPredicateM predOpt (fromDirTree "classes" . (tclss <>)  . fromJust . fromFileList . unCount) (counted extras) >>= \case
--         Just predicate -> do
--           rname <- view cfgReducerName
--           reduce rname Nothing predicate counted extras >>= \case
--             Just reduction -> do
--               return . fromJust . fromFileList . unCount $ reduction
--             Nothing -> do
--               failwith "The reduced result did not satisfy the predicate (flaky?)"
--         Nothing ->
--           failwith "Could not satisify the predicate."
--     False -> return $ textra'

--   classreader <- preloadClasses
--   void . flip runCachedClassPoolT (defaultFromReader classreader) $ do
--     setupPredicate (classesToFiles textra) clss >>= \case
--       Just predicate -> do
--         t' <- fromMaybe (fromClasses clss) <$> classReduction predicate clss
--         liftRIO (runPredicateM predicate t') >>= \case
--           True -> outputResults workFolder textra (ccContent t')
--           False ->
--             failwith "The reduced result did not satisfy the predicate (flaky?)"
--       Nothing -> do
--         failwith "Could not satisfy the predicate."
--   where
--     failwith ::
--       (HasLogger env, MonadReader env m, MonadIO m)
--       => Builder.Builder
--       -> m a
--     failwith msg = do
--       L.err msg
--       liftIO $ exitWith (ExitFailure 1)

--     outputResults workFolder textra t' =
--       view cfgOutput >>= \case
--       Just output
--         | isJar output -> liftIO $ do
--             let tmpFolder = (workFolder </> output)
--             writeTreeWith copySame $ tmpFolder :/ classesToFiles textra t'
--             arc <- withCurrentDirectory tmpFolder $ do
--               addFilesToArchive [OptRecursive] emptyArchive ["."]
--             BL.writeFile output (fromArchive arc)
--         | otherwise -> liftIO $ do
--           writeTreeWith copySame $ output :/ classesToFiles textra t'
--       Nothing ->
--         L.warn "Did not output to output."

--     copySame fp = \case
--       SameAs fp' -> copyFile fp' fp
--       Content bs -> BL.writeFile fp bs

--     toClassList =
--       catMaybes
--       . map (\(a,b) -> (,b) <$> asClassName a)
--       . toFileList

--     classesToFiles textra clss =
--       textra <> classesToFiles' clss

--     classesToFiles' clss =
--       fromJust . fromFileList . map (_1 %~ relativePathOfClass) $ clss


-- type ClassFiles = ClassCounter [(ClassName, FileContent)]


-- setupPredicate ::
--   (HasLogger env, HasConfig env, MonadReader env m, MonadClassPool m, MonadIO m)
--   => ([(ClassName, FileContent)] -> DirTree FileContent)
--   -> [(ClassName, FileContent)]
--   -> m ( Maybe ( PredicateM (ReaderT env IO) ClassFiles ) )
-- setupPredicate classesToFiles classData = L.phase "Setup Predicate" $ do
--   predOpt <- view cfgPredicateOptions
--   liftRIO $
--     toPredicateM
--       predOpt
--       (fromDirTree "classes" . classesToFiles . ccContent)
--       $ fromClasses classData

-- classReduction ::
--   forall m env a.
--   (HasLogger env, HasConfig env, MonadReader env m, MonadClassPool m, MonadIO m)
--   => PredicateM (ReaderT env IO) (ClassCounter [(ClassName, a)])
--   -> [(ClassName, a)]
--   -> m (Maybe (ClassCounter [(ClassName, a)]))
-- classReduction predicate targets = L.phase "Class Reduction" $ do
--   (coreFp, flip shrink (map fst targets) -> grph) <- computeClassGraph
--   L.debug $ "Possible reduction left:" <-> display (graphSize grph)

--   rname <- view cfgReducerName

--   view cfgStrategy >>= \case
--     ByItem validate -> do
--       predicate' <- case validate of
--         NoValidation ->
--           return predicate

--         Reject -> do
--           return . PredicateM $ \x ->
--             if flip isClosedIn grph . map fst . ccContent $ x
--             then runPredicateM predicate x
--             else return False

--         Shrink -> do
--           return $
--             (computeClassCounter.toValues.collapse grph.map fst.ccContent)
--             `contramap` predicate

--       liftRIO
--         . reduce rname Nothing predicate' computeClassCounter $ targets
--         where
--           computeClassCounter =
--             (\a -> ClassCounter (length a) (length a) a) . (coreFp ++)

--     ByClosure ->  do
--       closures' <- computeClosuresOfGraph grph

--       liftRIO
--         . reduce rname (Just $ IS.size . IS.unions)
--             predicate (fromIntSet coreFp grph)
--         $ closures'

--   where
--     fromIntSet coreFp graph scc = do
--       let
--         is = IS.unions scc
--         cl = coreFp ++ (toValues $ labels (IS.toList is) graph)
--       ClassCounter
--         { ccClosures = length scc
--         , ccClasses = length cl
--         , ccContent = cl
--         }

--     computeClassGraph = L.phase "Compute Class Graph" $ do
--       (coreCls, grph) <- forwardRemove <$> mkClassGraph <*> view cfgCore
--       let coreFp = toValues coreCls
--       L.debug
--         $ "The core closure is" <-> display (length coreCls)
--         <-> "classes," <-> display (length coreFp)
--         <-> "in the target."
--       return (coreFp, grph)

--     computeClosuresOfGraph grph = L.phase "Compute closures of graph:" $ do
--       let closures' = closures grph
--       L.debug $ "Found" <-> display (length closures') <-> "SCCs."
--       return $!! closures'

--     graphSize = sumOf (grNodes . like (1 :: Int))

--     values = HashMap.fromList targets

--     toValues lst =
--       let keepThese = HashSet.fromList $ lst
--       in HashMap.toList (HashMap.intersection values (HashSet.toMap keepThese))


-- unpackTarget ::
--   (MonadReader s m, MonadIO m, HasLogger s, HasConfig s)
--   => FilePath -> m (DirTree FileContent, DirTree FileContent)
-- unpackTarget folder = do
--   L.phase ("Unpacking target to" <-> display folder) $ do
--     target <- view cfgTarget
--     dx <- doesDirectoryExist target
--     if dx
--       then liftIO $ do
--         _ :/ tree <- readTree target
--         writeTreeWith (flip copyFileWithMetadata) (folder :/ tree)
--       else liftIO $ do
--         arch <- toArchive <$> BL.readFile target
--         extractFilesFromArchive [OptDestination folder] arch

--     _ :/ tree <- liftIO $ fmap SameAs <$> readTree folder

--     return
--       ( filterTreeOnFiles isClass tree
--       , filterTreeOnFiles (not . isClass) tree
--       )
--   where
--     isClass fn = takeExtension fn == ".class"

-- liftRIO ::
--   (MonadReader env m, MonadIO m)
--   => ReaderT env IO a -> m a
-- liftRIO m = do
--   x <- ask
--   liftIO $ runReaderT m x

-- preloadClasses :: ReaderT Config IO ClassPreloader
-- preloadClasses = L.phase "Preloading Classes" $ do
--   cls <- createClassLoader
--   (classreader, numclasses) <- liftIO $ do
--     classreader <- preload cls
--     numclasses <- length <$> classes classreader
--     return (classreader, numclasses)
--   L.debug $ "Found" <-> display numclasses <-> "classes."
--   return classreader

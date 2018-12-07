{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

-- lens
import           Control.Lens

-- deepseq
import           Control.DeepSeq

-- zip-archive
import           Codec.Archive.Zip

-- mtl
import           Control.Monad.Reader

-- filepath
import           System.FilePath

-- optparse-applicative
import           Options.Applicative          as A

-- reduce
import           Data.Functor.Contravariant.PredicateM
import           Control.Reduce

-- bytestring
import qualified Data.ByteString.Lazy         as BL

-- reduce-util
import           Control.Reduce.Util
import           Control.Reduce.Util.Logger   as L hiding (update, view)
import           Control.Reduce.Util.OptParse
import           System.Directory.Tree

-- unordered-containers
import qualified Data.HashMap.Strict          as HashMap
import qualified Data.HashSet                 as HashSet

-- -- containers
-- import qualified Data.Map                     as Map
-- import qualified Data.Set                     as Set

-- text
-- import qualified Data.Text.Lazy.Builder as Builder

-- unliftio
import           UnliftIO.Directory

-- base
-- import           Data.Either
import           Data.Foldable
import qualified Data.List                    as List
import           Data.Maybe
import           System.Exit

-- jvhms
import           Jvmhs

newtype Showed x = Showed x

instance Show (Showed x) where
  showsPrec _ _ _ = "Unshowable"

data Config = Config
  { _cfgLogger         :: !Logger
  , _cfgCore           :: HashSet.HashSet ClassName
  , _cfgClassPath      :: [ FilePath ]
  , _cfgUseStdlib      :: Bool
  , _cfgJreFolder      :: (Maybe FilePath)
  , _cfgTarget         :: FilePath
  , _cfgReducerOptions :: !ReducerOptions
  , _cfgCheckOptions   :: !CheckOptions
  , _cfgCmdOptions     :: !(Showed CmdOptionWithoutFormat)
  } deriving (Show)

makeClassy ''Config

instance HasLogger Config where
  loggerL = cfgLogger

configParser :: Parser (IO Config)
configParser =
  mkConfig
    <$> parseLogger
    <*> many parseCore
    <*> (concat <$>
         many (
            splitClassPath
              <$> strOption
              (long "cp"
               <> metavar "CLASSPATH"
               <> help "the library classpath, of things not reduced.")
            ))
    <*> switch
    ( long "stdlib"
      <> help "load the standard library."
    )
    <*> asum
    [ Just <$> strOption
         ( long "jre"
           <> metavar "JRE"
           <> help "the location of the stdlib."
         )
    , pure Nothing
    ]
    <*> strOption
    ( short 't'
      <> long "target"
      <> metavar "FILE"
      <> help "the path to the jar or folder to reduce."
    )
    <*> parseReducerOptions "jreduce"
    <*> parseCheckOptions
    <*> parseCmdOptions
  where
    parseCore = strOption (
      short 'c'
      <> long "core"
      <> metavar "CORE"
      <> help "the core classes to not reduce."
      )

    mkConfig l cores cp stdlib jre target comRed co c = do
      classCore <- readClassNames cores
      red <- comRed
      return $ Config l classCore cp stdlib jre target red co (Showed c)

    readClassNames classnames' = do
      names <- fmap concat . forM classnames' $ \cn ->
        case cn of
          '@':filename -> lines <$> readFile filename
          _            -> return [cn]
      return . HashSet.fromList . map strCls $ names

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
  workFolder <- view $ cfgReducerOptions . to workFolder
  (ttree, textra) <- unpackTarget =<< makeAbsolute (workFolder </> "unpacked")
  let
    targets = catMaybes $
        (\(fn, a) -> (,a) <$> asClassName fn)
        <$> toFileList ttree
    classesToFiles clss =
      textra <> (
        fromJust . fromFileList . map (_1 %~ relativePathOfClass) $ clss
      )

  classreader <- preloadClasses

  void . flip runCachedClassPoolT (defaultFromReader classreader) $ do
    setupPredicate classesToFiles targets >>= \case
      Just predicate -> do
        -- liftRIO $ do
        --   createDirectory "predicate"
        --   withCurrentDirectory "predicate" $ do
        --     x <- runPredicateM predicate targets
        --     L.info $ "Running the predicate results in" <-> display x

        t' <- classReduction predicate targets
        return ()

      Nothing -> do
        L.err "Could not satisfy predicate."
        liftIO $ exitWith (ExitFailure 1)


classReduction ::
  forall m env a.
  (HasLogger env, HasConfig env, MonadReader env m, MonadClassPool m, MonadIO m)
  => PredicateM (ReaderT env IO) [(ClassName, a)]
  -> [(ClassName, a)]
  -> m (Maybe [(ClassName, a)])
classReduction predicate targets = L.phase "Class Reduction" $ do
  (coreFp, grph) <- L.phase "Compute Class Graph" $ do
    (coreCls, grph) <- forwardRemove <$> mkClassGraph <*> view cfgCore
    let coreFp = toValues coreCls
    L.debug
      $ "The core closure is" <-> display (length coreCls)
      <-> "classes," <-> display (length coreFp) <-> "in the target."
    return (coreFp, grph)

  let graphLeft = graphSize grph
  if graphLeft == 0
    then do
    L.debug $ "No further class reduction after core closure."
    return (Just targets)

    else do
    L.debug $ "Possible reduction left:" <-> display graphLeft
    partitions <- L.phase "Compute partition of graph:" $ do
      let partitions = partition grph
      L.debug $ "Found" <-> display (length partitions) <-> "SCC."
      return $!! partitions

    redOpt <- view cfgReducerOptions
    liftRIO $ do
      x <- reduce redOpt "class" (logAndTransform coreFp `contramapM` predicate) $ partitions
      return $ fmap (\a -> coreFp ++ toX a) x

  where
    graphSize = sumOf (grNodes . like (1 :: Int))

    values = HashMap.fromList targets

    toValues lst =
      let keepThese = HashSet.fromList $ lst
      in HashMap.toList (HashMap.intersection values (HashSet.toMap keepThese))

    logAndTransform coreFp res = do
      let x = toX res
      L.debug
        $ "Reducing to" <-> display (length res)
        <-> "SCC, containing" <-> display (length x)
        <-> "classes."
      return (coreFp ++ x)

    toX :: [([ClassName], [ClassName])] -> [(ClassName, a)]
    toX = toValues . List.concatMap (view _2)

    -- toSCCs name nm lst = do
    --   let
    --     (bad, Set.fromList -> required) =
    --       partitionEithers
    --       . toListOf (folded . to (\n -> maybe (Left n) Right $ Map.lookup n nm ))
    --       $ lst

    --   forM_ bad $ \cn -> do
    --     L.warn $ "Could not find"
    --       <-> name
    --       <-> "class:"
    --       <-> display (cn ^. fullyQualifiedName) <> "."

    --   L.debug $ "Found that" <-> name <-> "covers" <-> display (Set.size required) <-> "SCC."
    --   return required

setupPredicate ::
  (HasLogger env, HasConfig env, MonadReader env m, MonadClassPool m, MonadIO m)
  => ([(ClassName, FileContent)] -> DirTree FileContent)
  -> [(ClassName, FileContent)]
  -> m (Maybe (PredicateM (ReaderT env IO) [(ClassName, FileContent)]))
setupPredicate classesToFiles classData = L.phase "Setup Predicate" $ do
  checkOpt <- view cfgCheckOptions
  Showed (CmdOptionWithoutFormat cmdFn) <- view cfgCmdOptions
  workFolder <- view $ cfgReducerOptions . to workFolder

  cmd <- liftIO $ cmdFn (DirInput "classes")
  prd <- liftRIO $ toPredicateM checkOpt cmd workFolder (classesToFiles classData)

  return . fmap (contramap classesToFiles) $ prd

unpackTarget ::
  (MonadReader s m, MonadIO m, HasLogger s, HasConfig s)
  => FilePath -> m (DirTree FileContent, DirTree FileContent)
unpackTarget folder = do
  L.phase ("Unpacking target to" <-> display folder) $ do
    target <- view cfgTarget
    dx <- doesDirectoryExist target
    if dx
      then liftIO $ do
        _ :/ tree <- readTree target
        writeTreeWith copyFileWithMetadata (folder :/ tree)
      else liftIO $ do
        arch <- toArchive <$> BL.readFile target
        extractFilesFromArchive [OptDestination folder] arch

    -- TODO: Do not save files yet (problems with runtime annotation).
    -- saveClasses folder targets
    _ :/ tree <- liftIO $ fmap SameAs <$> readTree folder

    return
      ( filterTreeOnFiles isClass tree
      , filterTreeOnFiles (not . isClass) tree
      )
  where
    isClass fn = takeExtension fn == ".class"

liftRIO ::
  (MonadReader env m, MonadIO m)
  => ReaderT env IO a -> m a
liftRIO m = do
  x <- ask
  liftIO $ runReaderT m x

preloadClasses :: ReaderT Config IO ClassPreloader
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

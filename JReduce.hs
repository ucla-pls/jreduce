{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

-- lens
import           Control.Lens
-- import Control.Lens.TH

-- mtl
import           Control.Monad.Reader

-- filepath
import System.FilePath

-- optparse-applicative
import           Options.Applicative          as A

-- reduce
import           Control.Reduce

-- reduce-util
import           System.Directory.Tree
import           Control.Reduce.Util
import           Control.Reduce.Util.Logger   as L hiding (update, view)
import           Control.Reduce.Util.OptParse

-- unordered-containers
import qualified Data.HashSet                 as S
-- import qualified Data.HashMap.Strict as M

-- unliftio
import  UnliftIO
import  UnliftIO.Directory

-- base
import           Data.Foldable
import           Data.Maybe

-- jvhms
import           Jvmhs

newtype Showed x = Showed x

instance Show (Showed x) where
  showsPrec _ _ _ = "Unshowable"

data Config = Config
  { _cfgLogger         :: !Logger
  , _cfgCore           :: S.HashSet ClassName
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
      return . S.fromList . map strCls $ names

main :: IO ()
main = do
  cfg <- join . execParser $
    A.info (configParser <**> helper)
    ( fullDesc
    <> header "jreduce"
    <> progDesc "A command line tool for reducing java programs."
    )

  print cfg
  runReaderT run cfg

run :: ReaderT Config IO ()
run = do
  targets <- readTargetClasses
  classreader <- preloadClasses

  void . flip runCachedClassPoolT (defaultFromReader classreader) $ do
    predicate <- setupPredicate targets

    return ()

setupPredicate ::
  (HasLogger env, HasConfig env, MonadReader env m, MonadClassPool m, MonadIO m)
  => [ClassName] -> m (Maybe (PredicateM (ReaderT env IO) [(ClassName, FileContent)]))
setupPredicate targets = L.phase "Setup Predicate" $ do
  checkOpt <- view cfgCheckOptions
  Showed (CmdOptionWithoutFormat cmdFn) <- view cfgCmdOptions
  workFolder <- view $ cfgReducerOptions . to workFolder

  folder <- makeAbsolute $ workFolder </> "unpacked"
  tree <- unpackTargets folder

  cmd <- liftIO $ cmdFn (DirInput "classes")

  prd <- liftRIO $ do
    toPredicateM checkOpt cmd workFolder (dirTree tree)

  return $ fmap (contramap $ classesToFiles) prd

  where
    classesToFiles =
      fromJust . fromFileList . map (_1 %~ relativePathOfClass)

    unpackTargets folder =
      L.phase ("Unpacking target to" <-> display folder) $ do
        saveClasses folder targets
        liftIO $ fmap SameAs <$> readTree folder

liftRIO ::
  (MonadReader env m, MonadIO m)
  => ReaderT env IO a -> m a
liftRIO m = do
  x <- ask
  liftIO $ runReaderT m x

readTargetClasses ::
  (HasLogger env, HasConfig env, MonadReader env m, MonadIO m)
  => m [ClassName]
readTargetClasses = L.phase "Read Target Classses" $ do
  cls <- liftIO . classes . fromClassPathOnly =<< view (cfgTarget . to (:[]))
  L.debug $ "Found" <-> display (length cls) <-> "target classes."
  return $ map fst cls


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

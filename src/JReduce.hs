{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

module JReduce where

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

data Config = Config
  { _cfgLogger         :: !Logger
  , _cfgCore           :: HashSet.HashSet ClassName
  , _cfgClassPath      :: [ FilePath ]
  , _cfgUseStdlib      :: Bool
  , _cfgJreFolder      :: (Maybe FilePath)
  , _cfgTarget         :: FilePath
  , _cfgOutput         :: FilePath
  , _cfgReducerOptions :: !ReducerOptions
  , _cfgCheckOptions   :: !CheckOptions
  , _cfgCmdOptions     :: !CmdOptionWithoutFormat
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
    <*> strOption
    ( short 'o'
      <> long "output"
      <> metavar "FILE"
      <> help "the path output folder."
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

    mkConfig l cores cp stdlib jre target output comRed co c = do
      classCore <- readClassNames cores
      red <- comRed
      return $ Config l classCore cp stdlib jre target output red co c

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
        classReduction predicate targets >>= \case
          Just t' -> do
            output <- view cfgOutput
            liftIO . writeTreeWith copySame $
              output :/ (textra <> (fromJust $ fromFileList (map (over _1 relativePathOfClass) t')))
          Nothing -> do
            L.err "Could not reduce results."
            liftIO $ exitWith (ExitFailure 1)
      Nothing -> do
        L.err "Could not satisfy predicate."
        liftIO $ exitWith (ExitFailure 1)

  where
    copySame fp = \case
      SameAs fp' -> copyFile fp' fp
      Content bs -> BL.writeFile fp bs

classReduction ::
  forall m env a.
  (HasLogger env, HasConfig env, MonadReader env m, MonadClassPool m, MonadIO m)
  => PredicateM (ReaderT env IO) [(ClassName, a)]
  -> [(ClassName, a)]
  -> m (Maybe [(ClassName, a)])
classReduction predicate targets = L.phase "Class Reduction" $ do
  redOpt <- view cfgReducerOptions

  (coreFp, grph) <- computeClassGraph

  worked <- checkIfCoreSatisfiesPredicate redOpt coreFp

  if worked
    then do
    L.info $ "No further class reduction needed after core closure."
    return (Just coreFp)

    else do
    let grph' = shrink grph (map fst targets)
    L.debug $ "Possible reduction left:" <-> display (graphSize grph')

    partitions <- computePartitionOfGraph grph'

    liftRIO $ do
      x <- reduce redOpt "class" (logAndTransform coreFp `contramapM` predicate) $ partitions
      return $ fmap (\a -> coreFp ++ toX a) x

  where
    computeClassGraph = L.phase "Compute Class Graph" $ do
      (coreCls, grph) <- forwardRemove <$> mkClassGraph <*> view cfgCore
      let coreFp = toValues coreCls
      L.debug
        $ "The core closure is" <-> display (length coreCls)
        <-> "classes," <-> display (length coreFp) <-> "in the target."
      return (coreFp, grph)

    checkIfCoreSatisfiesPredicate redOpt coreFp =
      liftRIO . L.phase "Checking if core satisfies predicate" $ do
      let corefolder = (workFolder redOpt </> "class-core")
      createDirectoryIfMissing True corefolder
      worked <- withCurrentDirectory corefolder $
        runPredicateM predicate coreFp
      L.debug (if worked then "It did." else "It did not.")
      return worked

    computePartitionOfGraph grph = L.phase "Compute partition of graph:" $ do
      let partitions = partition grph
      L.debug $ "Found" <-> display (length partitions) <-> "SCCs."
      return $!! partitions

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

setupPredicate ::
  (HasLogger env, HasConfig env, MonadReader env m, MonadClassPool m, MonadIO m)
  => ([(ClassName, FileContent)] -> DirTree FileContent)
  -> [(ClassName, FileContent)]
  -> m (Maybe (PredicateM (ReaderT env IO) [(ClassName, FileContent)]))
setupPredicate classesToFiles classData = L.phase "Setup Predicate" $ do
  checkOpt <- view cfgCheckOptions
  CmdOptionWithoutFormat cmdFn <- view cfgCmdOptions
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

-- {-# LANGUAGE OverloadedStrings   #-}
--
--{-# LANGUAGE DeriveGeneric   #-}
-- {-# LANGUAGE QuasiQuotes         #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TemplateHaskell     #-}
-- module Main where
--
-- import qualified Data.IntSet as IS
-- import qualified Data.List as L
-- import           Data.List.Split
-- import           Data.Monoid
-- import qualified Data.Text as Text
-- import qualified Data.Text.IO as Text
-- import           Data.Time
-- import           Text.Printf
-- import qualified Data.ByteString.Lazy.Char8 as BS
-- import           Data.Word
-- import           Data.Time.Clock.POSIX
-- import qualified Data.Vector as V
-- import           Prelude hiding (log)
-- import           System.Console.Docopt
-- import           System.Directory
-- import           System.Environment (getArgs)
-- import           System.Exit
-- import           System.IO
-- import           System.IO.Temp
-- import           System.Process
-- import           System.Timeout
-- import           System.FilePath
-- import           System.Random
-- import           GHC.Generics (Generic)
--
-- -- unordered-containers
-- import qualified Data.HashSet as S
-- import qualified Data.HashMap.Strict as M
--
-- import           Data.Csv
-- import           Data.Csv.Builder
-- import qualified Data.Aeson as A
-- import qualified Data.ByteString.Lazy as BL
-- import qualified Data.ByteString.Builder as BL
--
-- import           Control.Monad
-- import           Control.Monad.Trans
-- import           Control.Monad.IO.Class
-- import           Control.Monad.Trans.Reader (runReaderT)
-- import           Control.Monad.Reader.Class
-- import           Control.Reduce
--
-- import           Control.Lens hiding (argument)
--
-- import Debug.Trace
--
-- import           Jvmhs
-- import           Jvmhs.Data.Named
-- import           Jvmhs.Analysis.Reduce
-- import           Control.Concurrent
--
-- patterns :: Docopt
-- patterns = [docopt|
-- jreduce version 0.0.1
--
-- Jreduce is a tool that can reduce a java program to a smaller program.
-- If a property have been specified then jreduce will try to reduce to
-- the smallest program that still uphold the property.
--
-- Usage:
--   jreduce ( -h | --help )
--   jreduce [-v | -vv | -vvv] [options] [<property>...]
--
-- Options:
--   --cp <classpath>           The classpath to search for classess
--   --stdlib                   Also include the stdlib (Don't do this)
--   --jre <jre>                The location of the stdlib
--   -W, --warn                 Warn about missing classes to the stderr
--   -o, --output <output>      Output folder or jar
--   -r, --reductor <reductor>  Reductor
--   -c, --core <core>          The core classes, that should not be removed
--   --stub <stub>              Stub-classes, can be used instead of loading the library
--   --work-dir <work-dir>      The directory to perform the work in
--   -K, --keep                 Keep temporary folders around
--   -t, --timeout <timeout>    Timeout in seconds
--   -m, --max-iterations <itr> Limit the tool to <itr> invocations of the property.
--   -v                         Be more verbose
--   -p, --phases <phases>      List the phases seperated by ':'. default: class:method
-- |]
--
-- data ReductorName
--   = DDMin
--   | VerifyDDMin
--   | GraphDDMin
--   | GraphSortDDMin
--   | GBiRed
--   deriving (Show, Eq)
--
-- data Phase
--   = ClassClosure
--   | MethodClosure
--   deriving (Show, Eq)
--
-- parseReductorName str =
--   case str of
--     "ddmin" -> return DDMin
--     "ddmin:verify" -> return VerifyDDMin
--     "ddmin:graph" -> return GraphDDMin
--     "ddmin:graph-sort" -> return GraphSortDDMin
--     "gbired" -> return GBiRed
--     _ -> error $ "Unknown reductor: " ++ str
--
-- -- | The config file dictates the execution of the program
-- data Config = Config
--   { _cfgClassPath      :: !ClassPath
--   , _cfgOutput         :: !(Maybe FilePath)
--   , _cfgClasses        :: !(S.HashSet ClassName)
--   , _cfgUseStdlib      :: !Bool
--   , _cfgWarn           :: !Bool
--   , _cfgVerbose        :: !Int
--   , _cfgReductor       :: !ReductorName
--   , _cfgWorkDir        :: !FilePath
--   , _cfgProgressFile   :: !FilePath
--   , _cfgJre            :: !(Maybe FilePath)
--   , _cfgProperty       :: ![String]
--   , _cfgKeepTempFolder :: !Bool
--   , _cfgTimeout        :: !(Maybe Double)
--   , _cfgLoggingIndent  :: !Int
--   , _cfgMaxIterations  :: !(Maybe Int)
--   , _cfgStubs          :: !HierarchyStubs
--   , _cfgPhases          :: ![Phase]
--   } deriving (Show)
--
-- makeLenses 'Config
--
-- getArgOrExit :: Arguments -> Option -> IO String
-- getArgOrExit = getArgOrExitWith patterns
--
-- parsePhases :: Arguments -> IO [Phase]
-- parsePhases args = do
--   forM (splitOn ":" $ getArgWithDefault args "class:method" (longOption "phases")) $ \p ->
--     case p of
--       "class" -> return ClassClosure
--       "method" -> return MethodClosure
--       _ -> error $ "Does not know phase " ++ show p
--
-- parseConfig :: Arguments -> IO Config
-- parseConfig args = do
--   classnames <- readClassNames
--   tmpfoldername <- getTempFolderName
--   reductor <- parseReductorName $ getArgWithDefault args "gbired" (longOption "reductor")
--
--   stubs :: HierarchyStubs <-
--     case getArg args (longOption "stub") of
--       Just fn -> do
--         x <- A.decode <$> BS.readFile fn
--         case x of
--           Just x -> do
--             hPutStrLn stderr "Loaded stub-file"
--             return x
--           Nothing -> fail $ "Could not decode " ++ fn
--       Nothing -> return $ mempty
--
--   phases <- parsePhases args
--
--   let cfg = Config
--               { _cfgClassPath =
--                   case concatMap splitClassPath $ getAllArgs args (longOption "cp") of
--                     [] -> ["."]
--                     as -> as
--               , _cfgOutput = getArg args (shortOption 'o')
--               , _cfgClasses = classnames
--               , _cfgWarn = isPresent args (longOption "warn")
--               , _cfgReductor = reductor
--               , _cfgUseStdlib = isPresent args (longOption "stdlib")
--               , _cfgJre = getArg args (longOption "jre")
--               , _cfgVerbose = getArgCount args (shortOption 'v')
--               , _cfgProperty = getAllArgs args (argument "property")
--               , _cfgProgressFile = progressFileFromWorkDir tmpfoldername
--               , _cfgWorkDir = tmpfoldername
--               , _cfgKeepTempFolder = isPresent args (longOption "keep")
--               , _cfgTimeout = read <$> getArg args (longOption "timeout")
--               , _cfgLoggingIndent = 0
--               , _cfgMaxIterations = read <$> getArg args (longOption "max-iterations")
--               , _cfgStubs = stubs
--               , _cfgPhases = phases
--               }
--   case getArg args (longOption "work-dir") of
--     Just wd -> return (
--       cfg { _cfgWorkDir = wd
--           , _cfgKeepTempFolder = True
--           , _cfgProgressFile = progressFileFromWorkDir wd
--           })
--     Nothing -> return cfg
--
--   where
--     getTempFolderName = do
--       tmpfolder <- getCanonicalTemporaryDirectory
--       x :: Word <- randomIO
--       return $ tmpfolder </> "jreduce" ++ printf "-%0x" x
--
--     progressFileFromWorkDir wd = wd </> "progress.csv"
--
--     classnames' = getAllArgs args $ longOption "core"
--     readClassNames :: IO (S.HashSet ClassName)
--     readClassNames = do
--       names <- fmap concat . forM classnames' $ \cn ->
--         case cn of
--           '@':filename -> lines <$> readFile filename
--           _ -> return [cn]
--       return . S.fromList . map strCls $ names
--
--
-- type Property m = (String -> m Bool)
--
-- setupProperty ::
--   forall m. (MonadClassPool m, MonadIO m, MonadReader Config m)
--   => m (Property m)
-- setupProperty = do
--   workdir <- view cfgWorkDir
--   exists <- liftIO $ doesDirectoryExist workdir
--   when exists . dieWith $ "Work directory " ++ show workdir ++ " exists, please delete directory."
--   liftIO $ createDirectoryIfMissing True workdir
--   pf <- view cfgProgressFile
--   liftIO $ BL.writeFile pf
--      (BL.toLazyByteString $ encodeHeader (headerOrder (undefined :: ProgressRecord)))
--   prop <- view cfgProperty
--   case prop of
--     cmd:cmdArgs -> do
--       cmd' <- liftIO $ do
--         cmd' <- makeAbsolute cmd
--         doesFileExist cmd' >>= \case
--           True -> return cmd'
--           False -> error $ "Executable does not exits: " ++ cmd
--       sem <- liftIO $ do
--         newMVar 0
--       info $ "Found property " ++ cmd
--       return $ runProperty cmd' cmdArgs workdir sem
--     _ ->
--       return $ const (return True)
--
--   where
--     pad m c xs = replicate (m - length xs) c ++ xs
--
--     runProperty :: String -> [String] -> FilePath -> MVar Int -> Property m
--     runProperty cmd cmdArgs workdir sem name = time "Running property" $ do
--       iteration <- liftIO $ modifyMVar sem (\i -> return (i + 1, i))
--       view cfgMaxIterations >>= \case
--         Just maxiter | iteration > maxiter -1 -> do
--           dieWith "Reached max iterations, failing predicate"
--         _ -> do
--           let
--             iterationname = pad 6 '0' (show iteration) ++ "-" ++ name
--             outputfolder = workdir </> iterationname
--
--           clsfolder <- saveClassesTo outputfolder
--
--           res <- invokeProperty cmd cmdArgs outputfolder
--
--           logProgress iterationname res
--
--           keepFolder <- view cfgKeepTempFolder
--           unless keepFolder $ do
--             info "Deleting folder"
--             liftIO $ removeDirectoryRecursive outputfolder
--
--           return res
--
--     invokeProperty :: String -> [String] -> FilePath -> m Bool
--     invokeProperty cmd cmdArgs outputfolder = do
--       tout <- view cfgTimeout
--       cfg <- ask
--       time "Invoking property" . liftIO $ do
--         hstdout <- openFile (outputfolder </> "stdout.log") WriteMode
--         hstderr <- openFile (outputfolder </> "stderr.log") WriteMode
--         withCreateProcess (
--           (proc cmd (cmdArgs ++ ["classes/"]))
--             { std_in = NoStream
--             , std_out = UseHandle hstdout
--             , std_err = UseHandle hstderr
--             , cwd = Just outputfolder
--             }) $
--           \_ _ _ ph -> do
--             case tout of
--               Just tout -> do
--                 maybeCode <- timeout (floor $ 1000000 * tout) $ waitForProcess ph
--                 case maybeCode of
--                   Nothing -> do
--                     runReaderT (info $ "Timed out property after " ++ show tout ++ "s") cfg
--                     terminateProcess ph
--                     return False
--                   Just ec ->
--                     return $ ec == ExitSuccess
--               Nothing -> do
--                 ec <- waitForProcess ph
--                 return $ ec == ExitSuccess
--
--     saveClassesTo outputfolder = do
--       let classesFolder = outputfolder ++ "/classes"
--       liftIO $ createDirectoryIfMissing True classesFolder
--       ncls <- L.length <$> allClassNames
--       time ("Saving " ++ show ncls ++ " classes to " ++ classesFolder) $
--         saveAllClasses classesFolder
--       return classesFolder
--
--     logProgress name succ = do
--       clss <- allClasses
--       let numClss = length clss
--           (Sum numInterfaces, Sum numImpls, Sum numMethods) =
--             foldMap (\cls -> ( Sum (if isInterface cls then 1 :: Int else 0)
--                       , Sum (cls^.classInterfaces.to S.size)
--                       , Sum (cls^.classMethods.to M.size)
--                       )) clss
--       time <- liftIO $ getPOSIXTime
--       let precord =
--             ProgressRecord
--               name
--               (realToFrac time :: Double)
--               numClss
--               numInterfaces
--               numImpls
--               numMethods
--               (if succ then Success else Fail)
--       pf <- view cfgProgressFile
--       liftIO $ BL.appendFile pf
--          (BL.toLazyByteString $ encodeDefaultOrderedNamedRecord precord)
--       info $ show precord
--
-- data Result = Success | Fail
--   deriving (Show, Eq)
--
-- instance ToField Result where
--   toField Success = "Success"
--   toField Fail = "Fail"
--
-- data ProgressRecord = ProgressRecord
--   { prStep :: String
--   , prTime :: Double
--   , prClasses :: Int
--   , prInterfaces :: Int
--   , prImpls :: Int
--   , prMethods :: Int
--   , prResult:: Result
--   } deriving (Show, Eq, Generic)
--
-- myOptions :: Options
-- myOptions = defaultOptions { fieldLabelModifier = drop 2}
--
-- instance ToNamedRecord ProgressRecord where
--   toNamedRecord = genericToNamedRecord myOptions
--
-- instance DefaultOrdered ProgressRecord where
--   headerOrder = genericHeaderOrder myOptions
--
-- reduce ::
--   (MonadReader Config m, MonadIO m, Ord a)
--   => (String -> Predicate [a] m)
--   -> Graph a e
--   -> m (Maybe [a])
-- reduce prop graph = do
--   red <- view cfgReductor
--   case red of
--     DDMin ->
--       unsafeDdmin (prop "ddmin") (graph ^.. grNodes)
--     VerifyDDMin -> do
--       let propv xs = do
--             if isClosedIn xs graph
--               then prop "ddmin:verify" xs
--               else return False
--       unsafeDdmin propv (graph ^.. grNodes)
--     GraphDDMin -> do
--       let
--         sets = closures graph
--         unset = map (^?! toLabel graph . _Just) . IS.toList . IS.unions
--         propi = prop "ddmin:graph" . unset
--       info $ "Found " ++ show (L.length sets) ++ " closures"
--       fmap unset <$> unsafeDdmin propi sets
--     GraphSortDDMin -> do
--       let
--         sets = closures graph
--         unset = map (^?! toLabel graph . _Just) . IS.toList . IS.unions
--         propi = prop "ddmin:graph-sort" . unset
--       info $ "Found " ++ show (L.length sets) ++ " closures"
--       fmap unset <$> unsafeDdmin propi (L.sortOn IS.size sets)
--     GBiRed -> do
--       let
--         sets = closures graph
--         unset = map (^?! toLabel graph . _Just) . IS.toList
--         propi = prop "gbired" . unset
--       info $ "Found " ++ show (L.length sets) ++ " closures"
--       fmap (unset . IS.unions) <$> setBinaryReduction propi sets
--
-- classClosure ::
--   (MonadClassPool m, MonadIO m, MonadReader Config m)
--   => Property m
--   -> m ()
-- classClosure property = time "class-closure" $ do
--   (found, missing) <-
--     time "Running class closure" $
--       computeClassClosure =<< view cfgClasses
--   info $ "Found " ++ show (S.size found) ++ " required classes and "
--           ++ show (S.size missing) ++ " missing classes."
--
--   clss <- allClassNames
--   graph <- mkClassGraph
--   keep <- reduce cproperty graph
--   case keep of
--     Just clss' -> onlyClasses clss'
--     Nothing -> do
--       info $ "Could not statisfy predicate"
--       onlyClasses found
--
--   void $ property "after-class-closure"
--   where
--     cproperty name clss =
--       cplocal $ do
--         onlyClasses clss
--         property $ "class-closure-" ++ name
--
-- methodClosure ::
--   (MonadClassPool m, MonadIO m, MonadReader Config m)
--   => Property m
--   -> m ()
-- methodClosure property = time "method-closure" $ do
--   (missed, hry) <- time "Calculate hierarchy" $
--      getHierarchy
--
--   methodids <- concat <$> mapClasses (requiredMethods hry)
--
--   (missing, graph) <- mkCallGraph hry
--
--   let (required, graph') = forwardRemove graph methodids
--
--   keep <- reduce (mproperty required) graph'
--   case keep of
--     Just methods ->
--       reduceto (required ++ methods)
--     Nothing -> do
--       info $ "Could not statisfy predicate"
--
--   void $ property "after-method-closure"
--   where
--     reduceto methods = do
--       info $ "Reduced to " ++ show (length methods)
--       let clsmp = M.fromListWith (S.union)
--             $ methods ^.. folded
--             . to (\c -> (c^._1, S.singleton $ c ^._2))
--       modifyClasses $ \c ->
--         let
--           methods = clsmp ^. at (c^.name) . folded
--         in Just (c & classMethods %~ flip M.difference (S.toMap methods))
--
--     mproperty required name methods = do
--       cplocal $ do
--         reduceto (required++ methods)
--         property ("method-closure-" ++ name)
--
--
-- runPhase property p =
--   case p of
--     ClassClosure ->
--       -- Run the class closure
--       classClosure property
--     MethodClosure ->
--       -- Run the method closure
--       methodClosure property
--
-- runJReduce :: RIO Config ()
-- runJReduce = time "jreduce" $ do
--   classreader <- time "Preloading classes" $ do
--     cls <- createClassLoader
--     liftIO $ preload cls
--
--   cnt <- liftIO $ length <$> classes classreader
--   info $ "Found " ++ show cnt ++ " classes."
--
--   void . flip runCachedClassPoolT (defaultFromReader classreader) $ do
--     property <- setupProperty
--
--     -- Test if the property have been correctly setup
--     b <- property "initial"
--     unless b $ dieWith "Property failed on initial classpath"
--
--     view cfgPhases >>= mapM_ (runPhase property)
--
--     property "final"
--     view cfgOutput >>= \case
--       Just fp ->
--         time "Saving classes" $ saveAllClasses fp
--       Nothing ->
--         info "Does not save classes"
--
--   where
--     handleFailedToLoad [] = return ()
--     handleFailedToLoad errs = do
--       log "Could not load the following classes"
--       mapM_ (\e -> log $ "  - " ++ show e) errs
--
-- dieWith :: (MonadIO m) => String -> m b
-- dieWith =
--   liftIO . die
--
-- info :: (MonadReader Config m, MonadIO m) => String -> m ()
-- info str = do
--   v <- view cfgVerbose
--   when (v > 0) $ log str
--
-- time :: (MonadReader Config m , MonadIO m) => String -> m a -> m a
-- time str m = do
--   v <- view cfgVerbose
--   if v > 0
--     then do
--       log str
--       t <- liftIO getPOSIXTime
--       a <- local (cfgLoggingIndent %~ (+1)) m
--       t' <- liftIO getPOSIXTime
--       log $ "done [" ++ show (t' - t) ++ "]"
--       return a
--     else m
--
-- log :: (MonadReader Config m, MonadIO m) => String -> m ()
-- log msg = do
--   i <- view cfgLoggingIndent
--   liftIO . hPutStrLn stderr $ concat (replicate i "| ") ++ msg
--
-- main :: IO ()
-- main = do
--   args' <- parseArgs patterns <$> getArgs
--   case args' of
--     Right args
--       | isPresent args (longOption "help")
--         || isPresent args (shortOption 'h') ->
--           exitWithUsage patterns
--       | otherwise -> do
--           cfg <- parseConfig args
--           runRIO runJReduce cfg
--     Left msg ->
--       print msg
--
-- -- | Create a class loader from the config
-- createClassLoader :: RIO Config ClassLoader
-- createClassLoader = RIO $ \cfg -> do
--   let cp = cfg ^. cfgClassPath
--   if cfg ^. cfgUseStdlib
--     then
--       case cfg ^. cfgJre of
--         Nothing ->
--           liftIO $ fromClassPath cp
--         Just jre ->
--           liftIO $ fromJreFolder cp jre
--     else
--       return $ ClassLoader [] [] cp
--
-- newtype RIO a b = RIO { runRIO :: a -> IO b }
--   deriving (Functor)
--
-- instance Applicative (RIO a) where
--   pure = RIO . const . pure
--   mf <*> mx =
--     RIO (\a -> runRIO mf a <*> runRIO mx a)
--
-- instance Monad (RIO a) where
--   m >>= f = RIO (\a -> do x <- runRIO m a; runRIO (f x) a)
--
-- instance MonadReader a (RIO a) where
--   reader f = RIO $ return . f
--   local fr m = RIO (runRIO m . fr)
--
-- instance MonadIO (RIO a) where
--   liftIO m = RIO (const m)
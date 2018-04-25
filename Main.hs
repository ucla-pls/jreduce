{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Data.Set              as S
import qualified Data.Text.IO          as Text
import qualified Data.Text             as Text
import System.IO
import           System.Console.Docopt
import           System.Environment    (getArgs)

import Control.Monad

import           Control.Lens          hiding (argument)
import           Jvmhs

patterns :: Docopt
patterns = [docopt|
jreduce version 0.0.1

Usage:
  jreduce ( -h | --help )
  jreduce [options] [-o <output>] [-] [<classname>...]

Options:
  --cp=<classpath>      The classpath to search for classess
  --stdlib              Also include the stdlib (Don't do this)
  --jre=<jre>           The location of the stdlib
  -W, --warn            Warn about missing classes to the stderr
|]

-- | The config file dictates the execution of the program
data Config = Config
  { _cfgClassPath :: ClassPath
  , _cfgOutput    :: Maybe FilePath
  , _cfgClasses   :: S.Set ClassName
  , _cfgUseStdlib :: Bool
  , _cfgWarn      :: Bool
  , _cfgJre       :: Maybe FilePath
  } deriving (Show)

makeLenses 'Config

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns

parseConfig :: Arguments -> IO Config
parseConfig args = do
  classnames <- readClassNames
  return $ Config
    { _cfgClassPath =
        case concatMap splitClassPath $ getAllArgs args (longOption "cp") of
          [] -> ["."]
          as -> as
    , _cfgOutput = getArg args (shortOption 'o')
    , _cfgClasses = classnames
    , _cfgWarn = isPresent args (longOption "warn")
    , _cfgUseStdlib = isPresent args (longOption "stdlib")
    , _cfgJre = getArg args (longOption "jre")
    }

  where
    classnames' = getAllArgs args $ argument "classname"
    readClassNames = do
      names <-
        if isPresent args (command "-")
        then do (classnames' ++) . lines <$> getContents
        else return classnames'
      return . S.fromList . map dotCls $ names


main :: IO ()
main = do
  args' <- parseArgs patterns <$> getArgs
  case args' of
    Right args
      | isPresent args (longOption "help") ->
        exitWithUsage patterns
      | otherwise -> do
        cfg <- parseConfig args
        classreader <- preload =<< createClassLoader cfg
        result <- flip runHierarchy' (emptyState classreader) $ do
          computeClassClosure (cfg^.cfgClasses)

        case result of
          Right ((found, missed), hs) -> do
            case cfg^.cfgOutput of
              Just fp ->
                savePartialHierarchyState fp found hs
              Nothing ->
                mapM_ (Text.putStrLn . view fullyQualifiedName) found
            when (cfg^.cfgWarn) $ do
              hPutStrLn stderr "Did not find these classes on the class path while reducing:"
              mapM_ ( Text.hPutStrLn stderr
                    . Text.append "  - "
                    . view fullyQualifiedName
                    ) missed
          Left msg ->
            error $ show msg

    Left msg -> do
      print msg

-- | Create a class loader from the config
createClassLoader :: Config -> IO ClassLoader
createClassLoader cfg
  | cfg ^. cfgUseStdlib =
    case cfg ^. cfgJre of
      Nothing ->
        fromClassPath (cfg ^. cfgClassPath)
      Just jre ->
        fromJreFolder (cfg ^. cfgClassPath) jre
  | otherwise =
    return $ ClassLoader [] [] (cfg ^. cfgClassPath)

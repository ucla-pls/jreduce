{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Data.Set              as S
import qualified Data.Text.IO          as Text
import           System.Console.Docopt
import           System.Environment    (getArgs)

import           Control.Lens          hiding (argument)
import           Jvmhs

patterns :: Docopt
patterns = [docopt|
jreduce version 0.0.1

Usage:
  jreduce [options] [-o <output>] <classname>

Options:
  --cp=<classpath>      The classpath to search for classess
  --stdlib              Also include the stdlib (Don't do this)
  --jre=<jre>           The location of the stdlib
|]

-- | The config file dictates the execution of the program
data Config = Config
  { _cfgClassPath :: ClassPath
  , _cfgOutput    :: Maybe FilePath
  , _cfgClassName :: ClassName
  , _cfgUseStdlib :: Bool
  , _cfgJre       :: Maybe FilePath
  } deriving (Show)

makeLenses 'Config

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns

parseConfig :: Arguments -> IO Config
parseConfig args = do
  cn <- strCls <$> getArgOrExit args (argument "classname")
  return $ Config
    { _cfgClassPath =
        case concatMap splitClassPath $ getAllArgs args (longOption "cp") of
          [] -> ["."]
          as -> as
    , _cfgOutput = getArg args (shortOption 'o')
    , _cfgClassName = cn
    , _cfgUseStdlib = isPresent args (longOption "stdlib")
    , _cfgJre = getArg args (longOption "jre")
    }

main :: IO ()
main = do
  cfg <- parseConfig =<< parseArgsOrExit patterns =<< getArgs
  classreader <- preload =<< createClassLoader cfg

  result <- flip runHierarchy' (emptyState classreader) $ do
    computeClassClosure (S.singleton $ cfg^.cfgClassName)

  case result of
    Right (clss, hs) ->
      case cfg^.cfgOutput of
        Just fp ->
          savePartialHierarchyState fp clss hs
        Nothing ->
          mapM_ (Text.putStrLn . view fullyQualifiedName) clss
    Left msg ->
      error $ show msg

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

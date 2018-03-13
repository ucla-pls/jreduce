{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where


import           Control.Monad.IO.Class
import           Data.Foldable
import qualified Data.Set               as S
import qualified Data.Text.IO           as Text
import           System.Console.Docopt
import           System.Environment     (getArgs)

import           Control.Lens           hiding (argument)
import           Jvmhs

patterns :: Docopt
patterns = [docopt|
jreduce version 0.0.1

Usage:
  jreduce [options] [-o <output>] <classname>

Options:
  --cp=<classpath>      The classpath to search for classess
|]

-- | The config file dictates the execution of the program
data Config = Config
  { _cfgClassPath :: ClassPath
  , _cfgOutput    :: Maybe FilePath
  , _cfgClassName :: ClassName
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
    }

main :: IO ()
main = do
  cfg <- parseConfig =<< parseArgsOrExit patterns =<< getArgs
  classreader <- preload =<< createClassLoader cfg

  _ <- runHierarchy classreader $ do
    clss <- computeClassClosure (S.singleton $ cfg^.cfgClassName)
    liftIO . forM_ clss $ \c ->
      Text.putStrLn $ classNameAsText c

  return ()

-- | Create a class loader from the config
createClassLoader :: Config -> IO ClassLoader
createClassLoader cfg =
  return $ ClassLoader [] [] (cfg ^. cfgClassPath)


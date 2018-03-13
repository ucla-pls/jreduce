{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where


import           System.Console.Docopt
import           System.Environment    (getArgs)

import Control.Lens

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
  { _cfgClassPath :: [FilePath]
  , _cfgOutput    :: Maybe FilePath
  } deriving (Show)

makeLenses 'Config

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns

parseConfig :: Arguments -> IO Config
parseConfig args =
  return $ Config
    { _cfgClassPath =
        case concatMap (split ':') $ getAllArgs args (longOption "cp") of
          [] -> ["."]
          as -> as
    , _cfgOutput = getArg args (shortOption 'o')
    }

main :: IO ()
main = do
  argv <- getArgs
  args <- parseArgsOrExit patterns argv

  cfg <- parseConfig args

  print cfg


-- | split splits a list on an element
-- >> split ':' "Hello:World"
-- [ "Hello", "World" ]
-- split :: Char -> [Char] -> [[Char]]
split :: (Eq a) => a -> ([a] -> [[a]])
split a = go []
  where
    go lst [] =
      [ reverse lst ]
    go lst (b':rest)
      | b' == a =
        reverse lst : go [] rest
      | otherwise =
        go (b':lst) rest

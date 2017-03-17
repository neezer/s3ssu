module Main where

import           Lib
import           System.Console.CmdArgs

main :: IO ()
main = do
  config <- cmdArgs argConfig
  config <- fillFromEnv config

  print config

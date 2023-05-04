module Main where

import System.Environment (getArgs)
import TS.Parser (parseTS)

main :: IO ()
main = do
  args <- getArgs
  tsFile <- readFile $ head args
  print $ parseTS tsFile
  return ()

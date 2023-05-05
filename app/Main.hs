module Main where

import CTL.Parser (parseCTL)
import Semantics (evaluate)
import System.Environment (getArgs)
import TS.Parser (parseTS)

main :: IO ()
main = do
  -- todo check args
  args <- getArgs
  tsFile <- readFile $ head args
  let ts = parseTS tsFile
  let formula = parseCTL $ args !! 1
  print $ evaluate ts formula
  return ()

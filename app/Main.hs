module Main where

import System.Environment (getArgs)
import TS.Parser (parseTS)

main :: IO ()
main = do
  args <- getArgs
  tsFile <- readFile $ head args
  let ts = parseTS tsFile
  let (states, actions, transitions, initialStates, atomicPropositions, _) = ts
  print states
  print actions
  print transitions
  print initialStates
  print atomicPropositions
  return ()

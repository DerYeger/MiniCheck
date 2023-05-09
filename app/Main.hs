module Main where

import CTL.Model (StateFormula)
import CTL.Parser (parseCTL)
import CTL.Validator (validateCTL)
import Semantics (evaluate)
import System.Environment (getArgs)
import TS.Model (TransitionSystem)
import TS.Parser (parseTS)
import TS.Validator (validateTS)

parse :: String -> String -> Either String (TransitionSystem, StateFormula)
parse tsFile formula = do
  ts <- parseTS tsFile
  f <- parseCTL formula
  return (ts, f)

runWithCTL :: String -> String -> Either String Bool
runWithCTL tsFile formula = do
  ts <- parseTS tsFile >>= validateTS
  f <- parseCTL formula >>= validateCTL ts
  return $ evaluate ts f

main :: IO ()
main = do
  -- todo check args
  args <- getArgs
  tsFile <- readFile $ head args
  let formula = args !! 1
  let result = runWithCTL tsFile formula
  case result of
    Left err -> putStrLn err
    Right True -> putStrLn "The formula holds."
    Right False -> putStrLn "The formula does not hold."
  return ()

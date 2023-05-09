module Main where

import CTL.Model (StateFormula)
import CTL.Parser (parseCTL)
import Semantics (evaluate)
import System.Environment (getArgs)
import TS.Model (TransitionSystem)
import TS.Parser (parseTS)
import Validator (validate)

parse :: String -> String -> Either String (TransitionSystem, StateFormula)
parse tsFile formula = do
  ts <- parseTS tsFile
  f <- parseCTL formula
  return (ts, f)

parseValidateAndEvaluate :: String -> String -> Either String Bool
parseValidateAndEvaluate tsFile formula = do
  ts <- parseTS tsFile
  f <- parseCTL formula
  (ts', f') <- validate ts f
  return $ evaluate ts' f'

main :: IO ()
main = do
  -- todo check args
  args <- getArgs
  tsFile <- readFile $ head args
  let formula = args !! 1
  let result = parseValidateAndEvaluate tsFile formula
  case result of
    Left err -> putStrLn err
    Right True -> putStrLn "The formula holds."
    Right False -> putStrLn "The formula does not hold."
  return ()

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import CTL.Parser (parseCTL)
import CTL.Semantics (evaluateCTL)
import CTL.Validator (validateCTL)
import LTL.Parser (parseLTL)
import LTL.Semantics (evaluateLTL)
import LTL.Validator (validateLTL)
import System.Console.CmdArgs hiding (args)
import TS.Parser (parseTS)
import TS.Validator (validateTS)

data MiniCheck
  = Validate
      { tsFile :: FilePath
      }
  | CTL
      { tsFile :: FilePath,
        formula :: String
      }
  | LTL
      { tsFile :: FilePath,
        formula :: String,
        steps :: Int
      }
  deriving (Show, Data, Typeable)

tsValidatorArgs :: MiniCheck
tsValidatorArgs =
  Validate
    { tsFile = def &= typ "TS_FILE" &= argPos 0
    }

ctlModelCheckerArgs :: MiniCheck
ctlModelCheckerArgs =
  CTL
    { tsFile = def &= typ "TS_FILE" &= argPos 1,
      formula = def &= typ "CTL_FORMULA" &= argPos 2
    }

ltlModelCheckerArgs :: MiniCheck
ltlModelCheckerArgs =
  LTL
    { tsFile = def &= typ "TS_FILE" &= argPos 3,
      formula = def &= typ "LTL_FORMULA" &= argPos 4,
      steps = def &= typ "NATURAL_NUMBER" &= argPos 5
    }

runWithCTL :: String -> String -> Either String Bool
runWithCTL t f = do
  ts' <- parseTS t >>= validateTS
  f' <- parseCTL f >>= validateCTL ts'
  return $ evaluateCTL ts' f'

runWithLTL :: Int -> String -> String -> Either String Bool
runWithLTL k t f = do
  ts' <- parseTS t >>= validateTS
  f' <- parseLTL f >>= validateLTL ts'
  return $ evaluateLTL ts' f' k

main :: IO ()
main = do
  args <- cmdArgs $ modes [ctlModelCheckerArgs, ltlModelCheckerArgs, tsValidatorArgs]
  tsRaw <- readFile $ tsFile args
  case args of
    Validate {} -> do
      case parseTS tsRaw of
        Left err -> putStrLn err
        Right ts' -> do
          case validateTS ts' of
            Left err -> putStrLn err
            Right _ -> putStrLn "The transition system is valid."
    CTL {formula} -> do
      let result = runWithCTL tsRaw formula
      case result of
        Left err -> putStrLn err
        Right True -> putStrLn "The CTL formula holds."
        Right False -> putStrLn "The CTL formula does not hold."
    LTL {formula, steps} -> do
      let result = runWithLTL steps tsRaw formula
      case result of
        Left err -> putStrLn err
        Right True -> putStrLn "The LTL formula holds."
        Right False -> putStrLn "The LTL formula does not hold."
  return ()

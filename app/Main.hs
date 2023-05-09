{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -fno-cse #-}

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
    { formula = def &= typ "CTL_FORMULA" &= argPos 1
    }

ltlModelCheckerArgs :: MiniCheck
ltlModelCheckerArgs =
  LTL
    { formula = def &= typ "LTL_FORMULA" &= argPos 1,
      steps = def &= typ "BOUND" &= argPos 2
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

getCmdArgs :: Mode (CmdArgs MiniCheck)
getCmdArgs = cmdArgsMode $ modes [tsValidatorArgs, ctlModelCheckerArgs, ltlModelCheckerArgs]

main :: IO ()
main = do
  args <- cmdArgsRun getCmdArgs
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
        Right True -> putStrLn "The CTL formula is satisfied."
        Right False -> putStrLn "The CTL formula is not satisfied."
    LTL {formula, steps} -> do
      if steps < 1
        then putStrLn "The bound must be a positive integer."
        else do
          let result = runWithLTL steps tsRaw formula
          case result of
            Left err -> putStrLn err
            Right True -> putStrLn "The LTL formula is satisfied."
            Right False -> putStrLn "The LTL formula is not satisfied."
  return ()

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
import System.Exit (exitFailure, exitSuccess)
import TS.Parser (parseTS)
import TS.Validator (validateTS)

-- | CLI arguments.
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

-- | CLI arguments for the transition system validator.
tsValidatorArgs :: MiniCheck
tsValidatorArgs =
  Validate
    { tsFile = def &= typ "TS_FILE" &= argPos 0
    }

-- | CLI arguments for the CTL model checker.
ctlModelCheckerArgs :: MiniCheck
ctlModelCheckerArgs =
  CTL
    { formula = def &= typ "CTL_FORMULA" &= argPos 1
    }

-- | CLI arguments for the LTL model checker.
ltlModelCheckerArgs :: MiniCheck
ltlModelCheckerArgs =
  LTL
    { formula = def &= typ "LTL_FORMULA" &= argPos 1,
      steps = def &= typ "BOUND" &= argPos 2
    }

-- | Run the model checker with the given CTL formula.
runWithCTL :: String -> String -> Either String Bool
runWithCTL t f = do
  ts' <- parseTS t >>= validateTS
  f' <- parseCTL f >>= validateCTL ts'
  return $ evaluateCTL ts' f'

-- | Run the model checker with the given LTL formula.
runWithLTL :: Int -> String -> String -> Either String Bool
runWithLTL k t f = do
  ts' <- parseTS t >>= validateTS
  f' <- parseLTL f >>= validateLTL ts'
  return $ evaluateLTL ts' f' k

-- | Get the CLI arguments.
getCmdArgs :: Mode (CmdArgs MiniCheck)
getCmdArgs = cmdArgsMode $ modes [tsValidatorArgs, ctlModelCheckerArgs, ltlModelCheckerArgs]

-- | If an error is passed, print it and exit. Otherwise return the data.
exitOnErr :: Either String a -> IO a
exitOnErr (Left err) = do
  putStrLn err
  exitFailure
exitOnErr (Right x) = return x

-- | Main function.
main :: IO ()
main = do
  args <- cmdArgsRun getCmdArgs
  tsRaw <- readFile $ tsFile args
  case args of
    Validate {} -> do
      ts' <- exitOnErr $ parseTS tsRaw >>= validateTS
      putStrLn "The transition system is valid.\n"
      print ts'
      exitSuccess
    CTL {formula} -> do
      result <- exitOnErr $ runWithCTL tsRaw formula
      (if result then putStrLn "The CTL formula is satisfied." >> exitSuccess else putStrLn "The CTL formula is not satisfied." >> exitFailure)
    LTL {formula, steps} -> do
      if steps < 1
        then putStrLn "The bound must be a positive integer." >> exitFailure
        else do
          result <- exitOnErr $ runWithLTL steps tsRaw formula
          (if result then putStrLn "The LTL formula is satisfied." >> exitSuccess else putStrLn "The LTL formula is not satisfied." >> exitFailure)

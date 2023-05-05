module CTL.Parser (parseCTL) where

import CTL.Model
import Text.Parsec (choice, string)
import Text.Parsec.String (Parser)
import Utils (runParser)

parseCTL :: String -> StateFormula
parseCTL = runParser stateFormula

stateFormula :: Parser StateFormula
stateFormula = do
  choice [booleanLiteral]

booleanLiteral :: Parser StateFormula
booleanLiteral = do
  _ <- string "true"
  return (BoolLiteral True)

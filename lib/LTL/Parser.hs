module LTL.Parser (parseLTL) where

import LTL.Model
import Text.Parsec (choice, letter, many, space, string)
import Text.Parsec.String (Parser)
import Utils (runParser)
import Prelude hiding (until)

parseLTL :: String -> Either String Formula
parseLTL = runParser formula

formula :: Parser Formula
formula = choice [negation, bracketedFormula, booleanLiteral, prop]

booleanLiteral :: Parser Formula
booleanLiteral = do
  _ <- string "true"
  return (BoolLiteral True)

bracketedFormula :: Parser Formula
bracketedFormula = do
  _ <- string "("
  inner <- choice [timeFormula, binaryFormula]
  _ <- string ")"
  return inner

binaryFormula :: Parser Formula
binaryFormula = do
  left <- formula
  _ <- space
  operator <- parseOperator
  _ <- space
  operator left <$> formula

parseOperator :: Parser (Formula -> Formula -> Formula)
parseOperator = choice [conjunction, disjunction, implication, equivalence, xor, until]
  where
    conjunction = string "&&" >> return Conjunct
    disjunction = string "||" >> return transformDisjunction
    implication = string "->" >> return transformImplication
    equivalence = string "<->" >> return transformEquivalence
    xor = string "xor" >> return transformXor
    until = string "U" >> return Until

prop :: Parser Formula
prop = do
  p <- many letter
  return (Prop p)

negation :: Parser Formula
negation = do
  _ <- string "!("
  inner <- formula
  _ <- string ")"
  return (Negation inner)

timeFormula :: Parser Formula
timeFormula = choice [next, eventually, always]

next :: Parser Formula
next = do
  _ <- string "X "
  Next <$> formula

eventually :: Parser Formula
eventually = do
  _ <- string "F "
  transformEventually <$> formula

always :: Parser Formula
always = do
  _ <- string "G "
  transformAlways <$> formula

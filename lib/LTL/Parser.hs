-- | This module contains the parser for LTL formulas.
module LTL.Parser (parseLTL) where

import LTL.Model
import Text.Parsec (choice, letter, many, space, string)
import Text.Parsec.String (Parser)
import Utils (runParser)
import Prelude hiding (until)

-- | Parse an LTL formula from a string.
parseLTL :: String -> Either String Formula
parseLTL = runParser formula

-- | Parse an LTL formula.
formula :: Parser Formula
formula = choice [negation, bracketedFormula, booleanLiteral, prop]

-- | Parse a boolean literal.
booleanLiteral :: Parser Formula
booleanLiteral = do
  _ <- string "true"
  return (BoolLiteral True)

-- | Parse a bracketed formula.
bracketedFormula :: Parser Formula
bracketedFormula = do
  _ <- string "("
  inner <- choice [timeFormula, binaryFormula]
  _ <- string ")"
  return inner

-- | Parse a binary formula.
binaryFormula :: Parser Formula
binaryFormula = do
  left <- formula
  _ <- space
  operator <- parseOperator
  _ <- space
  operator left <$> formula

-- | Parse a binary operator, excluding its arguments.
parseOperator :: Parser (Formula -> Formula -> Formula)
parseOperator = choice [conjunction, disjunction, implication, equivalence, xor, until]
  where
    conjunction = string "&&" >> return Conjunct
    disjunction = string "||" >> return transformDisjunction
    implication = string "->" >> return transformImplication
    equivalence = string "<->" >> return transformEquivalence
    xor = string "xor" >> return transformXor
    until = string "U" >> return Until

-- | Parse an atomic proposition.
prop :: Parser Formula
prop = do
  p <- many letter
  return (Prop p)

-- | Parse a negation.
negation :: Parser Formula
negation = do
  _ <- string "!("
  inner <- formula
  _ <- string ")"
  return (Negation inner)

-- | Parse a time formula.
timeFormula :: Parser Formula
timeFormula = choice [next, eventually, always]

-- | Parse a next operator.
next :: Parser Formula
next = do
  _ <- string "X "
  Next <$> formula

-- | Parse an eventually operator.
eventually :: Parser Formula
eventually = do
  _ <- string "F "
  transformEventually <$> formula

-- | Parse an always operator.
always :: Parser Formula
always = do
  _ <- string "G "
  transformAlways <$> formula

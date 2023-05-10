-- | This module contains the parser for CTL formulas.
module CTL.Parser (parseCTL) where

import CTL.Model
import Text.Parsec (choice, letter, many, space, string)
import Text.Parsec.String (Parser)
import Utils (runParser)

-- | Parse a CTL formula from a string.
parseCTL :: String -> Either String StateFormula
parseCTL = runParser stateFormula

-- | Parse a state formula.
stateFormula :: Parser StateFormula
stateFormula = choice [exists, forAll, negation, binaryOperator, booleanLiteral, prop]

booleanLiteral :: Parser StateFormula
booleanLiteral = do
  _ <- string "true"
  return (BoolLiteral True)

-- | Parse a bracketed binary operator, including its arguments.
binaryOperator :: Parser StateFormula
binaryOperator = do
  _ <- string "("
  left <- stateFormula
  _ <- space
  operator <- parseOperator
  _ <- space
  right <- stateFormula
  _ <- string ")"
  return (operator left right)

-- | Parse a binary operator, excluding its arguments.
parseOperator :: Parser (StateFormula -> StateFormula -> StateFormula)
parseOperator = choice [conjunction, disjunction, implication, equivalence, xor]
  where
    conjunction = string "&&" >> return Conjunct
    disjunction = string "||" >> return transformDisjunction
    implication = string "->" >> return transformImplication
    equivalence = string "<->" >> return transformEquivalence
    xor = string "xor" >> return transformXor

-- | Parse an atomic proposition.
prop :: Parser StateFormula
prop = do
  p <- many letter
  return (Prop p)

-- | Parse a negation.
negation :: Parser StateFormula
negation = do
  _ <- string "!("
  inner <- stateFormula
  _ <- string ")"
  return (Negation inner)

-- | Parse an existential quantifier.
exists :: Parser StateFormula
exists = do
  _ <- string "E "
  (inner, flipOuter) <- pathFormula
  return (if flipOuter then Negation (ForAll inner) else Exists inner)

-- | Parse a universal quantifier.
forAll :: Parser StateFormula
forAll = do
  _ <- string "A "
  (inner, negateOuter) <- pathFormula
  return (if negateOuter then Negation (Exists inner) else ForAll inner)

-- | Parse a path formula. NOTE: The outer quantifier must be transformed if the second element is True.
pathFormula :: Parser (PathFormula, Bool)
pathFormula = do
  _ <- string "("
  inner <- choice [next, eventually, always, CTL.Parser.until]
  _ <- string ")"
  return inner

-- | Parse a next operator. NOTE: The outer quantifier must be transformed if the second element is True.
next :: Parser (PathFormula, Bool)
next = do
  _ <- string "X "
  inner <- stateFormula
  return (Next inner, False)

-- | Parse an eventually operator. NOTE: The outer quantifier must be transformed if the second element is True.
eventually :: Parser (PathFormula, Bool)
eventually = do
  _ <- string "F "
  inner <- stateFormula
  return (transformEventually inner, False)

-- | Parse an always operator. NOTE: The outer quantifier must be transformed if the second element is True.
always :: Parser (PathFormula, Bool)
always = do
  _ <- string "G "
  inner <- stateFormula
  return (transformAlways inner, True)

-- | Parse an until operator. NOTE: The outer quantifier must be transformed if the second element is True.
until :: Parser (PathFormula, Bool)
until = do
  left <- stateFormula
  _ <- string " U "
  right <- stateFormula
  return (Until left right, False)

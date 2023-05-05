module CTL.Parser (parseCTL) where

import CTL.Model
import Text.Parsec (choice, letter, many, space, string)
import Text.Parsec.String (Parser)
import Utils (runParser)

parseCTL :: String -> StateFormula
parseCTL = runParser stateFormula

stateFormula :: Parser StateFormula
stateFormula = choice [exists, forAll, negation, binaryOperator, booleanLiteral, prop]

booleanLiteral :: Parser StateFormula
booleanLiteral = do
  _ <- string "true"
  return (BoolLiteral True)

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

parseOperator :: Parser (StateFormula -> StateFormula -> StateFormula)
parseOperator = choice [conjunction, disjunction, implication, equivalence, xor]
  where
    conjunction = string "&&" >> return Conjunct
    disjunction = string "||" >> return transformDisjunction
    implication = string "->" >> return transformImplication
    equivalence = string "<->" >> return transformEquivalence
    xor = string "xor" >> return transformXor

prop :: Parser StateFormula
prop = do
  p <- many letter
  return (Prop p)

negation :: Parser StateFormula
negation = do
  _ <- string "!("
  inner <- stateFormula
  _ <- string ")"
  return (Negation inner)

exists :: Parser StateFormula
exists = do
  _ <- string "E "
  (inner, negateOuter) <- pathFormula
  return (if negateOuter then Negation (Exists inner) else Exists inner)

forAll :: Parser StateFormula
forAll = do
  _ <- string "A "
  (inner, negateOuter) <- pathFormula
  return (if negateOuter then Negation (ForAll inner) else ForAll inner)

pathFormula :: Parser (PathFormula, Bool)
pathFormula = do
  _ <- string "("
  inner <- choice [next, eventually, always, CTL.Parser.until]
  _ <- string ")"
  return inner

next :: Parser (PathFormula, Bool)
next = do
  _ <- string "X "
  inner <- stateFormula
  return (Next inner, False)

eventually :: Parser (PathFormula, Bool)
eventually = do
  _ <- string "F "
  inner <- stateFormula
  return (transformEventually inner, False)

always :: Parser (PathFormula, Bool)
always = do
  _ <- string "G "
  inner <- stateFormula
  return (transformAlways inner, True)

until :: Parser (PathFormula, Bool)
until = do
  left <- stateFormula
  _ <- string " U "
  right <- stateFormula
  return (Until left right, False)

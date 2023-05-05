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
    disjunction = string "||" >> return (\f1 f2 -> Negation (Conjunct (Negation f1) (Negation f2)))
    implication = string "->" >> return (Conjunct . Negation)
    equivalence = string "<->" >> return (\f1 f2 -> Conjunct (Conjunct (Negation f1) f2) (Conjunct (Negation f2) f1))
    xor = string "xor" >> return (\f1 f2 -> Conjunct (Conjunct (Negation f1) f2) (Conjunct (Negation f2) f1))

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
  Exists <$> pathFormula

forAll :: Parser StateFormula
forAll = do
  _ <- string "A "
  ForAll <$> pathFormula

pathFormula :: Parser PathFormula
pathFormula = do
  _ <- string "("
  inner <- choice [eventually, always, CTL.Parser.until]
  _ <- string ")"
  return inner

eventually :: Parser PathFormula
eventually = do
  _ <- string "F "
  Eventually <$> stateFormula

always :: Parser PathFormula
always = do
  _ <- string "G "
  Always <$> stateFormula

until :: Parser PathFormula
until = do
  left <- stateFormula
  _ <- string " U "
  Until left <$> stateFormula

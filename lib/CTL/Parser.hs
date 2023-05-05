module CTL.Parser (parseCTL) where

import CTL.Model
import Text.Parsec (choice, letter, many, string)
import Text.Parsec.String (Parser)
import Utils (runParser)

parseCTL :: String -> StateFormula
parseCTL = runParser stateFormula

stateFormula :: Parser StateFormula
stateFormula = choice [negation, conjunction, booleanLiteral, prop, exists, forAll]

booleanLiteral :: Parser StateFormula
booleanLiteral = do
  _ <- string "true"
  return (BoolLiteral True)

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

conjunction :: Parser StateFormula
conjunction = do
  _ <- string "("
  f1 <- stateFormula
  _ <- string " && "
  f2 <- stateFormula
  _ <- string ")"
  return (Conjunct f1 f2)

exists :: Parser StateFormula
exists = do
  _ <- string "E"
  _ <- string " "
  Exists <$> pathFormula

forAll :: Parser StateFormula
forAll = do
  _ <- string "A"
  _ <- string " "
  ForAll <$> pathFormula

pathFormula :: Parser PathFormula
pathFormula = choice [eventually, always, CTL.Parser.until]

eventually :: Parser PathFormula
eventually = do
  _ <- string "F"
  _ <- string " "
  Eventually <$> stateFormula

always :: Parser PathFormula
always = do
  _ <- string "G"
  _ <- string " "
  Always <$> stateFormula

until :: Parser PathFormula
until = do
  f1 <- stateFormula
  _ <- string " "
  _ <- string "U"
  _ <- string " "
  Until f1 <$> stateFormula

module CTL.Model where

data StateFormula = BoolLiteral Bool | Prop String | Conjunct StateFormula StateFormula | Negation StateFormula | Exists PathFormula | ForAll PathFormula deriving (Show, Eq, Ord)

data PathFormula = Eventually StateFormula | Always StateFormula | Until StateFormula StateFormula deriving (Show, Eq, Ord)

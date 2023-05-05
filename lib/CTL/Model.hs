module CTL.Model where

import TS.Model (AtomicProposition)

data StateFormula = BoolLiteral Bool | Prop AtomicProposition | Conjunct StateFormula StateFormula | Negation StateFormula | Exists PathFormula | ForAll PathFormula deriving (Show, Eq, Ord)

data PathFormula = Eventually StateFormula | Always StateFormula | Until StateFormula StateFormula deriving (Show, Eq, Ord)

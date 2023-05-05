module CTL.Model where

import TS.Model (AtomicProposition)

data StateFormula = TrueLiteral | Prop AtomicProposition | Conjunct StateFormula StateFormula | Negation StateFormula | Exists PathFormula | ForAll PathFormula

data PathFormula = Eventually StateFormula | Always StateFormula | Until StateFormula StateFormula

module CTL.Model where

data StateFormula = BoolLiteral Bool | Prop String | Conjunct StateFormula StateFormula | Negation StateFormula | Exists PathFormula | ForAll PathFormula deriving (Show, Eq, Ord)

data PathFormula = Next StateFormula | Until StateFormula StateFormula deriving (Show, Eq, Ord)

transformDisjunction :: StateFormula -> StateFormula -> StateFormula
transformDisjunction f1 f2 = Negation (Conjunct (Negation f1) (Negation f2))

transformImplication :: StateFormula -> StateFormula -> StateFormula
transformImplication f1 = transformDisjunction (Negation f1)

transformEquivalence :: StateFormula -> StateFormula -> StateFormula
transformEquivalence f1 f2 = Conjunct (transformImplication f1 f2) (transformImplication f2 f1)

transformXor :: StateFormula -> StateFormula -> StateFormula
transformXor f1 f2 = transformDisjunction (Conjunct f1 (Negation f2)) (Conjunct f2 (Negation f1))

transformEventually :: StateFormula -> PathFormula
transformEventually = Until (BoolLiteral True)

transformAlways :: StateFormula -> PathFormula
transformAlways = transformEventually . Negation

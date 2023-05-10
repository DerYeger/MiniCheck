-- | This module contains the data type for CTL formulas and some helper functions to transform them.
module CTL.Model where

-- | CTL formula.
data StateFormula = BoolLiteral Bool | Prop String | Conjunct StateFormula StateFormula | Negation StateFormula | Exists PathFormula | ForAll PathFormula deriving (Show, Eq, Ord)

-- | Path formula.
data PathFormula = Next StateFormula | Always StateFormula | Until StateFormula StateFormula deriving (Show, Eq, Ord)

-- | Transform a disjunction formula into a conjunction.
transformDisjunction :: StateFormula -> StateFormula -> StateFormula
transformDisjunction f1 f2 = Negation (Conjunct (Negation f1) (Negation f2))

-- | Transform an implication formula into a disjunction.
transformImplication :: StateFormula -> StateFormula -> StateFormula
transformImplication f1 = transformDisjunction (Negation f1)

-- | Transform an equivalence formula into a conjunction.
transformEquivalence :: StateFormula -> StateFormula -> StateFormula
transformEquivalence f1 f2 = Conjunct (transformImplication f1 f2) (transformImplication f2 f1)

-- | Transform an xor formula into a disjunction.
transformXor :: StateFormula -> StateFormula -> StateFormula
transformXor f1 f2 = transformDisjunction (Conjunct f1 (Negation f2)) (Conjunct f2 (Negation f1))

-- | Transform an eventually formula into an until formula.
transformEventually :: StateFormula -> PathFormula
transformEventually = Until (BoolLiteral True)

-- | Transform an always formula into an eventually formula. NOTE: The outer quantifier must still be transformed!
transformAlways :: StateFormula -> PathFormula
transformAlways inner = transformEventually (Negation inner)

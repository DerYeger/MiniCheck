-- | This module contains the data type for LTL formulas and some helper functions to transform them.
module LTL.Model where

-- | LTL formula.
data Formula = BoolLiteral Bool | Prop String | Conjunct Formula Formula | Negation Formula | Next Formula | Until Formula Formula deriving (Show, Eq, Ord)

-- | Transform a disjunction formula into a conjunction.
transformDisjunction :: Formula -> Formula -> Formula
transformDisjunction f1 f2 = Negation (Conjunct (Negation f1) (Negation f2))

-- | Transform an implication formula into a disjunction.
transformImplication :: Formula -> Formula -> Formula
transformImplication f1 = transformDisjunction (Negation f1)

-- | Transform an equivalence formula into a conjunction.
transformEquivalence :: Formula -> Formula -> Formula
transformEquivalence f1 f2 = Conjunct (transformImplication f1 f2) (transformImplication f2 f1)

-- | Transform an xor formula into a disjunction.
transformXor :: Formula -> Formula -> Formula
transformXor f1 f2 = transformDisjunction (Conjunct f1 (Negation f2)) (Conjunct f2 (Negation f1))

-- | Transform an eventually formula into an until formula.
transformEventually :: Formula -> Formula
transformEventually = Until (BoolLiteral True)

-- | Transform an always formula into an eventually formula.
transformAlways :: Formula -> Formula
transformAlways inner = Negation (transformEventually (Negation inner))

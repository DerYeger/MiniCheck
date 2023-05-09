module LTL.Model where

data Formula = BoolLiteral Bool | Prop String | Conjunct Formula Formula | Negation Formula | Next Formula | Always Formula | Until Formula Formula deriving (Show, Eq, Ord)

transformDisjunction :: Formula -> Formula -> Formula
transformDisjunction f1 f2 = Negation (Conjunct (Negation f1) (Negation f2))

transformImplication :: Formula -> Formula -> Formula
transformImplication f1 = transformDisjunction (Negation f1)

transformEquivalence :: Formula -> Formula -> Formula
transformEquivalence f1 f2 = Conjunct (transformImplication f1 f2) (transformImplication f2 f1)

transformXor :: Formula -> Formula -> Formula
transformXor f1 f2 = transformDisjunction (Conjunct f1 (Negation f2)) (Conjunct f2 (Negation f1))

transformEventually :: Formula -> Formula
transformEventually = Until (BoolLiteral True)

transformAlways :: Formula -> Formula
transformAlways inner = Negation (transformEventually (Negation inner))

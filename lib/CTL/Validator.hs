module CTL.Validator (validateCTL) where

import CTL.Model
import Data.Set (Set, empty, isSubsetOf, singleton, union)
import TS.Model

validateCTL :: TransitionSystem -> StateFormula -> Either String StateFormula
validateCTL (TS _ _ _ _ props _) f = if propSet f `isSubsetOf` props then Right f else Left "Formula contains undefined atomic propositions"

propSet :: StateFormula -> Set AtomicProposition
propSet (Prop name) = singleton (AtomicProposition name)
propSet (Negation f) = propSet f
propSet (Conjunct f1 f2) = propSet f1 `union` propSet f2
propSet (Exists f) = pathFormPropSet f
propSet (ForAll f) = pathFormPropSet f
propSet (BoolLiteral _) = empty

pathFormPropSet :: PathFormula -> Set AtomicProposition
pathFormPropSet (Next f) = propSet f
pathFormPropSet (Always f) = propSet f
pathFormPropSet (Until f1 f2) = propSet f1 `union` propSet f2

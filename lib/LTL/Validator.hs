-- | This module contains validation functions for LTL formulas.
module LTL.Validator (validateLTL) where

import Data.Set (Set, empty, isSubsetOf, singleton, union)
import LTL.Model
import TS.Model

-- | Validate a given LTL formula on a given transition system.
validateLTL :: TransitionSystem -> Formula -> Either String Formula
validateLTL (TS _ _ _ _ props _) f = if propSet f `isSubsetOf` props then Right f else Left "Formula contains undefined atomic propositions"

-- | Compute the set of atomic propositions in a given formula.
propSet :: Formula -> Set AtomicProposition
propSet (Prop name) = singleton (AtomicProposition name)
propSet (Negation f) = propSet f
propSet (Conjunct f1 f2) = propSet f1 `union` propSet f2
propSet (BoolLiteral _) = empty
propSet (Next f) = propSet f
propSet (Until f1 f2) = propSet f1 `union` propSet f2

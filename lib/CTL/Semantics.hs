-- | This module contains the semantics of CTL formulas.
module CTL.Semantics (evaluateCTL) where

import CTL.Model
import Data.Set (Set, empty, fromList, intersection, isSubsetOf, toList, union, (\\))
import TS.Model
import Utils (fSet, post)

-- | Evaluate a given CTL formula on a given transition system.
evaluateCTL :: TransitionSystem -> StateFormula -> Bool
evaluateCTL ts@(TS _ _ _ initialStates _ _) f = initialStates `isSubsetOf` satState ts f

-- | Compute the set of states satisfying a given state formula.
satState :: TransitionSystem -> StateFormula -> Set State
satState ts@(TS states _ _ _ _ labelingFunction) f = case f of
  BoolLiteral b -> if b then states else empty
  Prop p -> fromList $ filter (\s -> AtomicProposition p `elem` labelingFunction s) (toList states)
  Conjunct left right -> satState ts left `intersection` satState ts right
  Negation inner -> states \\ satState ts inner
  Exists inner -> satPath ts inner
  ForAll (Next inner) -> satState ts (Negation (Exists (Next (Negation inner))))
  ForAll (Until left right) -> satState ts (Conjunct (Negation (Exists (Until (Negation right) (Conjunct (Negation left) (Negation right))))) (Negation (Exists (Always (Negation right)))))
  _ -> error "Unsupported state formula. This is an internal error."

-- | Compute the set of states satisfying a given path formula.
satPath :: TransitionSystem -> PathFormula -> Set State
satPath ts@(TS states _ _ _ _ _) (Next inner) = fSet (\s -> (post ts s `intersection` satState ts inner) /= empty) states
satPath ts (Until left right) = smallestSet satRight
  where
    satRight = satState ts right
    satLeft = satState ts left
    smallestSet t = if candidates /= empty then smallestSet (t `union` candidates) else t
      where
        candidates = fSet (\s -> (post ts s `intersection` t) /= empty) (satLeft \\ t)
satPath ts (Always inner) = largestSet satInner
  where
    satInner = satState ts inner
    largestSet t = if candidates /= empty then t \\ candidates else t
      where
        candidates = fSet (\s -> (post ts s `intersection` t) == empty) t

module Semantics (evaluate) where

import CTL.Model
import Data.Set (Set, empty, fromList, intersection, isSubsetOf, toList, union, (\\))
import TS.Model
import Utils (fSet)

evaluate :: TransitionSystem -> StateFormula -> Bool
evaluate ts@(TS _ _ _ initialStates _ _) f = initialStates `isSubsetOf` satState ts f

satState :: TransitionSystem -> StateFormula -> Set State
satState ts@(TS states _ _ _ _ labelingFunction) f = case f of
  BoolLiteral b -> if b then states else empty
  Prop p -> fromList $ filter (\s -> AtomicProposition p `elem` labelingFunction s) (toList states)
  Conjunct left right -> satState ts left `intersection` satState ts right
  Negation inner -> states \\ satState ts inner
  Exists inner -> satPath ts inner
  ForAll (Next inner) -> satState ts (Negation (Exists (Next (Negation inner))))
  ForAll (Until left right) -> satState ts (Conjunct (Negation (Exists (Until (Negation right) (Conjunct (Negation left) (Negation right))))) (Negation (Exists (Always (Negation right)))))
  _ -> error "Unsupported state formula"

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

post :: TransitionSystem -> State -> Set State
post (TS _ _ transitions _ _ _) s = fromList $ map (\(T _ _ to) -> to) $ filter (\(T from _ _) -> from == s) transitions

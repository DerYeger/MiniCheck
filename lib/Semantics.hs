module Semantics (evaluate) where

import CTL.Model
import Data.Set (Set, empty, fromList, intersection, isSubsetOf, toList, (\\))
import TS.Model

evaluate :: TransitionSystem -> StateFormula -> Bool
evaluate ts@(TS _ _ _ initialStates _ _) f = initialStates `isSubsetOf` satState ts f

satState :: TransitionSystem -> StateFormula -> Set State
satState ts@(TS states _ _ _ _ labelingFunction) f = case f of
  BoolLiteral b -> if b then states else empty
  Prop p -> fromList $ filter (\s -> AtomicProposition p `elem` labelingFunction s) (toList states)
  Conjunct left right -> satState ts left `intersection` satState ts right
  Negation inner -> states \\ satState ts inner
  _ -> error "not yet implemented"

-- Exists p -> evaluatePathFormula ts p s
-- ForAll p -> complement (evaluatePathFormula ts p s)

satPath :: TransitionSystem -> PathFormula -> State -> State -> Bool
satPath _ _ _ _ = False

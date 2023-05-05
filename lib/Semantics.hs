module Semantics (evaluate) where

import CTL.Model
import Data.Set (Set, fromList, isSubsetOf, toList)
import TS.Model

evaluate :: TransitionSystem -> StateFormula -> Bool
evaluate ts@(TS states _ _ initialStates _ _) f = initialStates `isSubsetOf` satStates
  where
    satStates = fromList $ filter (evaluateStateFormula ts f) (toList states)

evaluateStateFormula :: TransitionSystem -> StateFormula -> State -> Bool
evaluateStateFormula _ (BoolLiteral b) _ = b
evaluateStateFormula ts (Prop p) s = AtomicProposition p `elem` getLabelingFunction ts s
evaluateStateFormula ts (Conjunct f1 f2) s = evaluateStateFormula ts f1 s && evaluateStateFormula ts f2 s
evaluateStateFormula ts (Negation f) s = not (evaluateStateFormula ts f s)
evaluateStateFormula ts (Exists p) s = any (evaluatePathFormula ts p s) (getStates ts)
evaluateStateFormula ts (ForAll p) s = all (evaluatePathFormula ts p s) (getStates ts)

getStates :: TransitionSystem -> Set State
getStates (TS states _ _ _ _ _) = states

getLabelingFunction :: TransitionSystem -> LabelingFunction
getLabelingFunction (TS _ _ _ _ _ labelingFunction) = labelingFunction

evaluatePathFormula :: TransitionSystem -> PathFormula -> State -> State -> Bool
evaluatePathFormula _ _ _ _ = False

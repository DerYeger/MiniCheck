module Semantics (evaluate) where

import CTL.Model
import Data.Set (Set)
import TS.Model

evaluate :: TransitionSystem -> StateFormula -> State -> Bool
evaluate _ (BoolLiteral b) _ = b
evaluate ts (Prop p) s = AtomicProposition p `elem` getLabelingFunction ts s
evaluate ts (Conjunct f1 f2) s = evaluate ts f1 s && evaluate ts f2 s
evaluate ts (Negation f) s = not (evaluate ts f s)
evaluate ts (Exists p) s = any (evaluatePathFormula ts p s) (getStates ts)
evaluate ts (ForAll p) s = all (evaluatePathFormula ts p s) (getStates ts)

getStates :: TransitionSystem -> Set State
getStates (TS states _ _ _ _ _) = states

getLabelingFunction :: TransitionSystem -> LabelingFunction
getLabelingFunction (TS _ _ _ _ _ labelingFunction) = labelingFunction

evaluatePathFormula :: TransitionSystem -> PathFormula -> State -> State -> Bool
evaluatePathFormula _ _ _ _ = False

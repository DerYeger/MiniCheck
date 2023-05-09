module Validator (validate) where

import CTL.Model
import Data.Set (Set, empty, isSubsetOf, singleton, union)
import TS.Model
import Utils (fSet)

validate :: TransitionSystem -> StateFormula -> Either String (TransitionSystem, StateFormula)
validate ts f = do
  ts' <- validateTS ts
  f' <- validateFormula ts' f
  return (ts', f')

validateTS :: TransitionSystem -> Either String TransitionSystem
validateTS ts = someStatesAreInitial ts >>= noStateIsTerminal >>= allStatesDefined

someStatesAreInitial :: TransitionSystem -> Either String TransitionSystem
someStatesAreInitial ts@(TS _ _ _ initialStates _ _) = if null initialStates then Left "There are no initial states." else Right ts

noStateIsTerminal :: TransitionSystem -> Either String TransitionSystem
noStateIsTerminal ts@(TS states _ transitions _ _ _) = if null terminalStates then Right ts else Left ("There are terminal states: " ++ show terminalStates ++ ".")
  where
    terminalStates = fSet stateIsTerminal states
    stateIsTerminal state = all (\(T from _ _) -> state /= from) transitions

allStatesDefined :: TransitionSystem -> Either String TransitionSystem
allStatesDefined ts@(TS states _ transitions _ _ _) = if null undefinedStates then Right ts else Left ("There are undefined states: " ++ show undefinedStates ++ ".")
  where
    undefinedStates = filter stateIsUndefined (concatMap (\(T from _ to) -> [from, to]) transitions)
    stateIsUndefined state = state `notElem` states

validateFormula :: TransitionSystem -> StateFormula -> Either String StateFormula
validateFormula (TS _ _ _ _ props _) f = if propSet f `isSubsetOf` props then Right f else Left "Formula contains undefined atomic propositions"

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

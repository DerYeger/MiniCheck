module TS.Validator (validateTS) where

import TS.Model
import Utils (fSet)

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

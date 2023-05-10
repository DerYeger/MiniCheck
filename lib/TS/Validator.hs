-- | This module contains validation functions for transition systems.
module TS.Validator (validateTS) where

import TS.Model
import Utils (fSet)

-- | Validate a given transition system.
validateTS :: TransitionSystem -> Either String TransitionSystem
validateTS ts = someStatesAreInitial ts >>= noStateIsTerminal >>= allStatesDefined

-- | Check if there are some initial states.
someStatesAreInitial :: TransitionSystem -> Either String TransitionSystem
someStatesAreInitial ts@(TS _ _ _ initialStates _ _) = if null initialStates then Left "There are no initial states." else Right ts

-- | Check if there are no terminal states.
noStateIsTerminal :: TransitionSystem -> Either String TransitionSystem
noStateIsTerminal ts@(TS states _ transitions _ _ _) = if null terminalStates then Right ts else Left ("There are terminal states: " ++ show terminalStates ++ ".")
  where
    terminalStates = fSet stateIsTerminal states
    stateIsTerminal state = all (\(T from _ _) -> state /= from) transitions

-- | Check if all states used in transitions are defined.
allStatesDefined :: TransitionSystem -> Either String TransitionSystem
allStatesDefined ts@(TS states _ transitions _ _ _) = if null undefinedStates then Right ts else Left ("There are undefined states: " ++ show undefinedStates ++ ".")
  where
    undefinedStates = filter stateIsUndefined (concatMap (\(T from _ to) -> [from, to]) transitions)
    stateIsUndefined state = state `notElem` states

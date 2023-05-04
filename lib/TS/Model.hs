module TS.Model where

newtype State = State String deriving (Show, Eq)

newtype Action = Action {actionName :: String} deriving (Show, Eq)

newtype AtomicProposition = AtomicProposition {propName :: String} deriving (Show, Eq)

type LabelingFunction = State -> [AtomicProposition]

type TransitionSystem = ([State], [Action], [(State, Action, State)], [State], [AtomicProposition], LabelingFunction)

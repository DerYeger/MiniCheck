module TS.Model where

newtype State = State String deriving (Show, Eq)

newtype Action = Action String deriving (Show, Eq)

newtype AtomicProposition = AtomicProposition String deriving (Show, Eq)

type LabelingFunction = State -> [AtomicProposition]

type Transition = (State, Action, State)

type TransitionSystem = ([State], [Action], [Transition], [State], [AtomicProposition], LabelingFunction)

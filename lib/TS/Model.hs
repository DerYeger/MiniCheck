module TS.Model where

import Data.Set (Set)

newtype State = State String deriving (Show, Eq, Ord)

newtype Action = Action String deriving (Show, Eq, Ord)

newtype AtomicProposition = AtomicProposition String deriving (Show, Eq, Ord)

type LabelingFunction = State -> [AtomicProposition]

type Transition = (State, Action, State)

type TransitionSystem = (Set State, Set Action, [Transition], Set State, Set AtomicProposition, LabelingFunction)

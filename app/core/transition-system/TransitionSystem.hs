module TransitionSystem where

data State = State {name :: String} deriving (Show, Eq)

data Action = Action {name :: String} deriving (Show, Eq)

data AtomicProposition = AtomicProposition {name :: String} deriving (Show, Eq)

type LabelingFunction = State -> [AtomicProposition]

type TransitionSystem = ([State], [Action], [(State, Action, State)], [State], [AtomicProposition], LabelingFunction)

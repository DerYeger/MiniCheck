-- | This module contains the parser for transition systems.
module TS.Parser (parseTS) where

import Data.List (find)
import Data.Set (fromList)
import TS.Model
import Text.Parsec (char, endBy, letter, many, newline, option, sepBy, space, string, (<|>))
import Text.Parsec.String (Parser)
import Utils (runParser)

-- | Parse a transition system from a string.
parseTS :: String -> Either String TransitionSystem
parseTS = runParser ts

-- | Parse a transition system.
ts :: Parser TransitionSystem
ts = do
  _ <- string "States:" >> newline
  statesWithLabels <- parseState `endBy` newline
  _ <- newline
  transitions <- parseTransitions
  let initialStates = map (\(state, _, _) -> state) $ filter (\(_, initial, _) -> initial) statesWithLabels
  let states = map (\(state, _, _) -> state) statesWithLabels
  let atomicPropositions = concatMap (\(_, _, labels) -> labels) statesWithLabels
  let actions = map (\(T _ action _) -> action) transitions
  let labelingFunction = createLabelingFunction statesWithLabels
  return (TS (fromList states) (fromList actions) transitions (fromList initialStates) (fromList atomicPropositions) labelingFunction)

-- | Create a labeling function from a list of states with labels.
createLabelingFunction :: [(State, Bool, [AtomicProposition])] -> LabelingFunction
createLabelingFunction statesWithLabels state = maybe [] (\(_, _, labels) -> labels) (find (\(s, _, _) -> s == state) statesWithLabels)

-- | Parse an arrow.
arrow :: Parser String
arrow = string "->"

-- | Parse a list item start, i.e, dash.
listItemStart :: Parser String
listItemStart = string "-"

-- | Parse a label, i.e., an atomic proposition.
parseLabel :: Parser AtomicProposition
parseLabel = do
  name <- many letter
  return (AtomicProposition name)

-- | Parse a list of labels, i.e., atomic propositions.
parseLabels :: Parser [AtomicProposition]
parseLabels = do
  _ <- string ": "
  parseLabel `sepBy` string ", "

-- | Parse a state.
parseState :: Parser (State, Bool, [AtomicProposition])
parseState = do
  _ <- listItemStart
  initial <- option False (string ">" >> return True)
  _ <- space
  name <- many letter
  labels <- option [] parseLabels
  return (State name, initial, AtomicProposition name : labels)

-- | Parse a list of transitions.
parseTransitions :: Parser [Transition]
parseTransitions = do
  _ <- string "Transitions:" >> newline
  parseTransition `endBy` newline

-- | Parse an action name.
actionName :: Parser String
actionName = many (letter <|> char '_')

-- | Parse a transition.
parseTransition :: Parser Transition
parseTransition = do
  _ <- listItemStart >> space
  from <- many letter
  _ <- space >> arrow >> space
  action <- actionName
  _ <- space >> arrow >> space
  to <- many letter
  return (T (State from) (Action action) (State to))

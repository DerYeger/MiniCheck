module TS.Parser (parseTS) where

import Data.List (find)
import Data.Set (fromList)
import TS.Model
import Text.Parsec (char, endBy, letter, many, newline, option, sepBy, space, string, (<|>))
import Text.Parsec.String (Parser)
import Utils (runParser)

parseTS :: String -> TransitionSystem
parseTS = runParser ts

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

createLabelingFunction :: [(State, Bool, [AtomicProposition])] -> LabelingFunction
createLabelingFunction statesWithLabels state = maybe [] (\(_, _, labels) -> labels) (find (\(s, _, _) -> s == state) statesWithLabels)

arrow :: Parser String
arrow = string "->"

listItemStart :: Parser String
listItemStart = string "-"

parseLabel :: Parser AtomicProposition
parseLabel = do
  name <- many letter
  return (AtomicProposition name)

parseLabels :: Parser [AtomicProposition]
parseLabels = do
  _ <- string ": "
  parseLabel `sepBy` string ", "

parseState :: Parser (State, Bool, [AtomicProposition])
parseState = do
  _ <- listItemStart >> space
  name <- many letter
  initial <- option False (string " (init)" >> return True)
  labels <- option [] parseLabels
  return (State name, initial, AtomicProposition name : labels)

parseTransitions :: Parser [Transition]
parseTransitions = do
  _ <- string "Transitions:" >> newline
  parseTransition `endBy` newline

actionName :: Parser String
actionName = many (letter <|> char '_')

parseTransition :: Parser Transition
parseTransition = do
  _ <- listItemStart >> space
  from <- many letter
  _ <- space >> arrow >> space
  action <- actionName
  _ <- space >> arrow >> space
  to <- many letter
  return (T (State from) (Action action) (State to))

module TS.Parser (parseTS) where

import Data.Set (fromList)
import TS.Model
import Text.Parsec (char, letter, many, newline, option, space, string, (<|>))
import Text.Parsec.String (Parser)
import Utils (runParser)

parseTS :: String -> TransitionSystem
parseTS = runParser ts

ts :: Parser TransitionSystem
ts = do
  _ <- string "States:" >> newline
  statesWithLabels <- many parseState
  _ <- newline
  transitions <- parseTransitions
  let initialStates = map (\(state, _, _) -> state) $ filter (\(_, initial, _) -> initial) statesWithLabels
  let states = map (\(state, _, _) -> state) statesWithLabels
  let atomicPropositions = concatMap (\(_, _, labels) -> labels) statesWithLabels
  let actions = map (\(_, action, _) -> action) transitions
  let labelingFunction _ = [] -- todo
  return (fromList states, fromList actions, transitions, fromList initialStates, fromList atomicPropositions, labelingFunction)

arrow :: Parser String
arrow = string "->"

listItemStart :: Parser String
listItemStart = string "-"

parseInit :: Parser Bool
parseInit = do
  _ <- string " (init)"
  return True

parseState :: Parser (State, Bool, [AtomicProposition])
parseState = do
  _ <- listItemStart >> space
  name <- many letter
  initial <- option False parseInit
  _ <- newline
  return (State name, initial, [])

parseTransitions :: Parser [Transition]
parseTransitions = do
  _ <- string "Transitions:" >> newline
  many parseTransition

actionName :: Parser String
actionName = do
  many (letter <|> char '_')

parseTransition :: Parser Transition
parseTransition = do
  _ <- listItemStart >> space
  from <- many letter
  _ <- space >> arrow >> space
  action <- actionName
  _ <- space >> arrow >> space
  to <- many letter
  _ <- newline
  return (State from, Action action, State to)

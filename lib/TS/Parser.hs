module TS.Parser (parseTS) where

import TS.Model
import Text.Parsec (char, letter, many, newline, space, string, (<|>))
import Text.Parsec.String (Parser)
import Utils (runParser)

parseTS :: String -> TransitionSystem
parseTS = runParser ts

ts :: Parser TransitionSystem
ts = do
  (initialState, states) <- parseStates
  _ <- newline
  transitions <- parseTransitions
  let actions = map (\(_, action, _) -> action) transitions
  let atomicPropositions = [] -- todo
  let labelingFunction _ = [] -- todo
  return (states, actions, transitions, [initialState], atomicPropositions, labelingFunction)

parseStates :: Parser (State, [State])
parseStates = do
  _ <- string "States:" >> newline
  initialState <- parseInitialState
  otherStates <- many parseState
  return (initialState, initialState : otherStates)

arrow :: Parser String
arrow = string "->"

listItemStart :: Parser String
listItemStart = string "-"

parseInitialState :: Parser State
parseInitialState = do
  _ <- arrow >> space
  name <- many letter
  _ <- newline
  return (State name)

parseState :: Parser State
parseState = do
  _ <- listItemStart >> space
  name <- many letter
  _ <- newline
  return (State name)

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

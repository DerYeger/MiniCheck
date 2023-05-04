module TS.Parser (parseTS) where

import TS.Model
import Text.Parsec (letter, many, newline, space, string)
import Text.Parsec.String (Parser)
import Utils (runParser)

parseTS :: String -> State
parseTS = runParser parseStates

parseStates :: Parser State
parseStates = keyword >> parseInitialState
  where
    keyword = do
      _ <- string "States:"
      newline

arrow :: Parser String
arrow = string "->"

parseInitialState :: Parser State
parseInitialState = do
  _ <- arrow >> space
  name <- many letter
  _ <- newline
  return (State name)

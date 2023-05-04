module Utils where

import Text.Parsec (parse)
import Text.Parsec.String (Parser)

runParser :: Parser a -> String -> a
runParser p str = case parse p "" str of
  Left err -> error $ "parse error at " ++ show err
  Right val -> val

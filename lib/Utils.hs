module Utils (runParser) where

import Text.Parsec (eof, parse)
import Text.Parsec.String (Parser)

withEof :: Parser a -> Parser a
withEof p = do
  result <- p
  eof
  return result

runParser :: Parser a -> String -> a
runParser p str = case parse (withEof p) "" str of
  Left err -> error $ "parse error at " ++ show err
  Right val -> val

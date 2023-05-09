module Utils (runParser, fSet) where

import Data.Set (Set, fromList, toList)
import Text.Parsec (eof, parse)
import Text.Parsec.String (Parser)

withEof :: Parser a -> Parser a
withEof p = do
  result <- p
  eof
  return result

runParser :: Parser a -> String -> Either String a
runParser p s = case parse (withEof p) "" s of
  Left err -> Left (show err)
  Right result -> Right result

fSet :: Ord a => (a -> Bool) -> Set a -> Set a
fSet f = fromList . filter f . toList

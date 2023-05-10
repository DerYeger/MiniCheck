-- | This module contains utility functions used by the other modules.
module Utils (runParser, fSet, post) where

import Data.Set (Set, fromList, toList)
import TS.Model
import Text.Parsec (eof, parse)
import Text.Parsec.String (Parser)

-- | Extend a given parser to ensure the entire input is consumed.
withEof :: Parser a -> Parser a
withEof p = do
  result <- p
  eof
  return result

-- | Run a parser on a string, returning either an error message or the result.
runParser :: Parser a -> String -> Either String a
runParser p s = case parse (withEof p) "" s of
  Left err -> Left (show err)
  Right result -> Right result

-- | Filter a set using a predicate. The set will be transformed to an intermediate list.
fSet :: Ord a => (a -> Bool) -> Set a -> Set a
fSet f = fromList . filter f . toList

-- | Compute the post set, i.e., the successors, of a given state.
post :: TransitionSystem -> State -> Set State
post (TS _ _ transitions _ _ _) s = fromList $ map (\(T _ _ to) -> to) $ filter (\(T from _ _) -> from == s) transitions

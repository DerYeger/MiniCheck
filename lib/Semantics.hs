module Semantics (evaluate) where

import CTL.Model
import Data.Set (Set, empty, fromList, intersection, isSubsetOf, toList, (\\))
import TS.Model

evaluate :: TransitionSystem -> StateFormula -> Bool
evaluate ts@(TS _ _ _ initialStates _ _) f = initialStates `isSubsetOf` satState ts f

satState :: TransitionSystem -> StateFormula -> Set State
satState ts@(TS states _ _ _ _ labelingFunction) f = case f of
  BoolLiteral b -> if b then states else empty
  Prop p -> fromList $ filter (\s -> AtomicProposition p `elem` labelingFunction s) (toList states)
  Conjunct left right -> satState ts left `intersection` satState ts right
  Negation inner -> states \\ satState ts inner
  _ -> error "not yet implemented"

-- satPath :: TransitionSystem -> PathFormula -> Set State
-- satPath ts@(TS states _ _ _ _ labelingFunction) f = case f of
--   Next inner -> fromList $ concatMap (\s -> transition ts s (Action "")) (toList states)
--   Until left right -> error "not yet implemented"

-- transition :: TransitionSystem -> State -> Action -> [State]
-- transition (TS _ _ transitions _ _ _) s a = map (\(T _ _ to) -> to) $ filter (\(T from action _) -> from == s && action == a) transitions

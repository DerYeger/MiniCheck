-- | This module contains the semantics of LTL formulas.
module LTL.Semantics (evaluateLTL, boundedPaths, trace, Path, Trace) where

import Data.Set (toList)
import LTL.Model
import TS.Model
import Utils (post)
import Prelude hiding (pi)

-- | Evaluate a given LTL formula on a given transition system and bound value.
evaluateLTL :: TransitionSystem -> Formula -> Int -> Bool
evaluateLTL ts f k = all (sat ts f) sigma
  where
    sigma = map (trace ts) pi
    pi = boundedPaths ts k

-- | Evaluate if a given formula is satisfied by a given trace.
sat :: TransitionSystem -> Formula -> Trace -> Bool
sat ts f sigma = case f of
  BoolLiteral b -> b
  Prop p -> not (null sigma) && AtomicProposition p `elem` head sigma
  Negation inner -> not (sat ts inner sigma)
  Conjunct left right -> sat ts left sigma && sat ts right sigma
  Next inner -> not (null sigma) && sat ts inner (tail sigma)
  Until left right -> any (\j -> sat ts right (drop j sigma) && all (\i -> sat ts left (drop i sigma)) [0 .. j - 1]) [0 .. length sigma - 1]

-- | A path is a list of states.
type Path = [State]

-- | Compute the set of all paths of a given transition system up to a given length.
boundedPaths :: TransitionSystem -> Int -> [Path]
boundedPaths ts@(TS _ _ _ initialStates _ _) k = map reverse (paths (map return (toList initialStates)) 1)
  where
    paths :: [Path] -> Int -> [Path]
    paths ps l
      | l == k = ps
      | otherwise = paths (concatMap successors ps) (l + 1)
    successors p = map (: p) (toList (post ts (head p)))

-- | A trace is a list of lists of atomic propositions.
type Trace = [[AtomicProposition]]

-- | Compute the trace of a given path.
trace :: TransitionSystem -> Path -> Trace
trace (TS _ _ _ _ _ labelingFunction) = map labelingFunction

module LTL.Semantics (evaluateLTL, boundedPaths, trace) where

import Data.Set (toList)
import LTL.Model
import TS.Model
import Utils (post)
import Prelude hiding (pi)

evaluateLTL :: TransitionSystem -> Formula -> Int -> Bool
evaluateLTL ts f k = all (sat ts f) sigma
  where
    sigma = map (trace ts) pi
    pi = boundedPaths ts k

sat :: TransitionSystem -> Formula -> Trace -> Bool
sat ts f sigma = case f of
  BoolLiteral b -> b
  Prop p -> AtomicProposition p `elem` head sigma
  Negation inner -> not (sat ts inner sigma)
  Conjunct left right -> sat ts left sigma && sat ts right sigma
  Next inner -> sat ts inner (tail sigma)
  Until left right -> any (\j -> sat ts right (drop j sigma) && all (\i -> sat ts left (drop i sigma)) [0 .. j - 1]) [0 .. length sigma - 1]
  _ -> error "Unsupported LTL formula"

type Path = [State]

boundedPaths :: TransitionSystem -> Int -> [Path]
boundedPaths ts@(TS _ _ _ initialStates _ _) k = map reverse (paths (map return (toList initialStates)) 1)
  where
    paths :: [Path] -> Int -> [Path]
    paths ps l
      | l == k = ps
      | otherwise = paths (concatMap successors ps) (l + 1)
    successors p = map (: p) (toList (post ts (head p)))

type Trace = [[AtomicProposition]]

trace :: TransitionSystem -> Path -> Trace
trace (TS _ _ _ _ _ labelingFunction) = map labelingFunction

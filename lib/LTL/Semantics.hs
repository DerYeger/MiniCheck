module LTL.Semantics (evaluateLTL) where

import Data.Set (Set, empty, fromList, intersection, isSubsetOf, toList, (\\))
import LTL.Model
import TS.Model

evaluateLTL :: TransitionSystem -> Formula -> Bool
evaluateLTL ts@(TS _ _ _ initialStates _ _) f = initialStates `isSubsetOf` satState ts f

satState :: TransitionSystem -> Formula -> Set State
satState ts@(TS states _ _ _ _ labelingFunction) f = empty

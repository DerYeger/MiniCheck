{-# LANGUAGE InstanceSigs #-}

module TS.Model where

import Data.List (intercalate)
import Data.Set (Set, toList)

newtype State = State String deriving (Show, Eq, Ord)

newtype Action = Action String deriving (Show, Eq, Ord)

newtype AtomicProposition = AtomicProposition String deriving (Show, Eq, Ord)

type LabelingFunction = State -> [AtomicProposition]

data Transition = T State Action State deriving (Show, Eq, Ord)

data TransitionSystem = TS (Set State) (Set Action) [Transition] (Set State) (Set AtomicProposition) LabelingFunction

instance Show TransitionSystem where
  show :: TransitionSystem -> String
  show (TS states _ transitions initialStates atomicPropositions labelingFunction) = "States:\n" ++ showStates ++ "\nTransitions:\n" ++ showTransitions
    where
      showStates = unlines $ map showState $ toList states
      showState (State name) = "- " ++ name ++ (if State name `elem` toList initialStates then " (init)" else "") ++ showLabels name
      showLabels name = case filter (/= AtomicProposition name) $ labelingFunction (State name) of
        [] -> ""
        labels -> ": " ++ intercalate ", " (map (\(AtomicProposition prop) -> prop) $ filter (\prop -> prop `elem` toList atomicPropositions) $ labels)
      showTransitions = unlines $ map showTransition transitions
      showTransition (T (State from) (Action action) (State to)) = "- " ++ from ++ " -> " ++ action ++ " -> " ++ to

instance Eq TransitionSystem where
  (==) :: TransitionSystem -> TransitionSystem -> Bool
  (TS states1 actions1 transitions1 initialStates1 atomicPropositions1 labelingFunction1) == (TS states2 actions2 transitions2 initialStates2 atomicPropositions2 labelingFunction2) =
    states1 == states2
      && actions1 == actions2
      && transitions1 == transitions2
      && initialStates1 == initialStates2
      && atomicPropositions1 == atomicPropositions2
      && all (\state -> labelingFunction1 state == labelingFunction2 state) states1

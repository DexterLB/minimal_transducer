{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)

import Data.Hashable (hashWithSalt, Hashable)

main :: IO ()
main = putStrLn "Hello, Haskell!"

data State = State {
    sid :: Int,
    transition :: HashMap Char Int,
    final :: Maybe String,
    output :: HashMap Char String
} deriving (Eq)

data Trans = Trans {
    states :: HashMap Int State,
    equiv :: HashSet State,
    start :: Int
} deriving (Eq)

instance Hashable State where
    hashWithSalt salt State {sid, transition, final, output}
        = hashWithSalt salt (sid, transition, final, output)

instance Show Trans where
    show = (foldr (++) "") . (map (++ "\n")) . showTransLines

instance Show State where
    show = (foldr (++) "") . (map (++ "\n")) . showStateLines

showTransLines :: Trans -> [String]
showTransLines Trans {states, start}
    =  ["transducer with start " ++ (show start)]
    ++ (map ("  " ++) (foldr (++) [] (map showStateLines (HashMap.elems states))))

showStateLines :: State -> [String]
showStateLines State {sid, transition, final, output}
    =  ["state " ++ show sid ++ ":"]
    ++ ["  transitions:"]
    ++ (map ("    " ++) (showMapLines transition))
    ++ ["  outputs:"]
    ++ (map ("    " ++) (showMapLines output))
    ++ (showFinalLines final)

showMapLines :: (Show k, Show v) => HashMap k v -> [String]
showMapLines = (map showPair) . HashMap.toList
    where
        showPair (k, v) = (show k) ++ " -> " ++ (show v)

showFinalLines :: (Show o) => Maybe o -> [String]
showFinalLines Nothing = []
showFinalLines (Just output) = ["  final with output " ++ (show output)]


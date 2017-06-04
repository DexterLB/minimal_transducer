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

updateEquiv :: Trans -> Trans
updateEquiv Trans {states, start} = Trans {
    states = states,
    start = start,
    equiv = HashSet.fromList $ HashMap.elems states
}

-- **** Prints ****

instance Show Trans where
    show = (foldr (++) "") . (map (++ "\n")) . showTransLines

instance Show State where
    show = (foldr (++) "") . (map (++ "\n")) . showStateLines

showTransLines :: Trans -> [String]
showTransLines t
    =  (showTransDataLines t)
    ++ (showVerifyEquivLines t)
    ++ (showMismatchingStatesLines t)

showTransDataLines :: Trans -> [String]
showTransDataLines Trans {states, start}
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

showVerifyEquivLines :: Trans -> [String]
showVerifyEquivLines t
    | verifyEquiv t = []
    | otherwise     = ["  WARN: transducer has wrong equivalence set"]

showMismatchingStatesLines :: Trans -> [String]
showMismatchingStatesLines t
    | mis == [] = []
    | otherwise = ["  WARN: transducer has mismatching states with keys: " ++ (show mis)]
    where
        mis = mismatchingStates t

verifyEquiv :: Trans -> Bool
verifyEquiv Trans {states, equiv} = (HashSet.fromList (HashMap.elems states)) == equiv

mismatchingStates :: Trans -> [Int]
mismatchingStates Trans {states} = filter checkState (HashMap.keys states)
    where
        checkState k = (sid (states HashMap.! k)) /= k

showMapLines :: (Show k, Show v) => HashMap k v -> [String]
showMapLines = (map showPair) . HashMap.toList
    where
        showPair (k, v) = (show k) ++ " -> " ++ (show v)

showFinalLines :: (Show o) => Maybe o -> [String]
showFinalLines Nothing = []
showFinalLines (Just output) = ["  final with output " ++ (show output)]


instance Hashable State where
    hashWithSalt salt State {sid, transition, final, output}
        = hashWithSalt salt (sid, transition, final, output)

-- **** Data ****

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

juljul :: Trans
juljul = updateEquiv $ Trans {
    start = 0,
    states = HashMap.fromList [
        (0, State 
                0 
                (HashMap.fromList [
                    ('a', 5),
                    ('d', 6),
                    ('f', 13),
                    ('j', 11)
                ])
                Nothing
                (HashMap.fromList [
                    ('a', "3"),
                    ('d', "31"),
                    ('f', "2"),
                    ('j', "31")
                ])
        ),
        (5, State 
                5 
                (HashMap.fromList [
                    ('p', 2),
                    ('u', 3)
                ])
                Nothing
                (HashMap.fromList [
                    ('p', "0"),
                    ('u', "1")
                ])
        ),
        (2, State 
                2 
                (HashMap.fromList [
                    ('r', 1)
                ])
                Nothing
                (HashMap.fromList [
                ])
        ),
        (3, State 
                3 
                (HashMap.fromList [
                    ('g', 1)
                ])
                Nothing
                (HashMap.fromList [
                ])
        ),
        (6, State 
                6 
                (HashMap.fromList [
                    ('e', 4)
                ])
                Nothing
                (HashMap.fromList [
                ])
        ),
        (4, State 
                4 
                (HashMap.fromList [
                    ('c', 1)
                ])
                Nothing
                (HashMap.fromList [
                ])
        ),
        (13, State 
                13 
                (HashMap.fromList [
                    ('e', 12)
                ])
                Nothing
                (HashMap.fromList [
                ])
        ),
        (12, State 
                12 
                (HashMap.fromList [
                    ('b', 8)
                ])
                Nothing
                (HashMap.fromList [
                ])
        ),
        (1, State 
                1 
                (HashMap.fromList [
                    ('a', 9)
                ])
                Nothing
                (HashMap.fromList [
                ])
        ),
        (9, State 
                9 
                (HashMap.fromList [
                    ('n', 1)
                ])
                Nothing
                (HashMap.fromList [
                ])
        ),
        (1, State 
                1 
                (HashMap.fromList [
                ])
                (Just "")
                (HashMap.fromList [
                ])
        ),
        (8, State 
                8 
                (HashMap.fromList [
                ])
                (Just "[8|9]")
                (HashMap.fromList [
                ])
        )
    ],
    equiv = HashSet.fromList []
}


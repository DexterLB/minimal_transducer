{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Transducer where

import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)

import Data.Hashable (hashWithSalt, Hashable)

import Data.Maybe (fromJust)

-- **** traversing ****

-- | returns the output of the transducer for a given word
match :: Trans -> String -> Maybe String
match t w = makeOutput $ nextO' t (start t) w
    where
        makeOutput Nothing              = Nothing
        makeOutput (Just (n, sofar))    = finalOutput (final $ state t n) sofar

        finalOutput Nothing _           = Nothing
        finalOutput (Just suffix) sofar = Just $ sofar ++ suffix

-- | returns the state and output acquired by traversing with the given word
nextO' :: Trans -> Int -> String -> Maybe (Int, String)
nextO' t n w
    | length wordPath /= length w + 1 = Nothing
    | otherwise = Just (
            last wordPath,
            foldr (++) "" $ zipWith (outEmpty t) wordPath w
        )
    where
        wordPath = path t n w

-- | returns the state acquired by making transitions with the given word
next' :: Trans -> Int -> String -> Maybe Int
next' t n w
    | length wordPath == length w + 1   = Just (last wordPath)
    | otherwise                         = Nothing
    where
        wordPath = path t n w

-- | returns the path for traversing with the given word
path :: Trans -> Int -> String -> [Int]
path _ n ""     = [n]
path t n (a:w)  = makePath (next t n a)
    where
        makePath Nothing = []
        makePath (Just stateID) = n : (path t stateID w)

-- | returns the next state and output when performing a transition with the given symbol
nextO :: Trans -> Int -> Char -> Maybe (Int, String)
nextO t n a = stateOutput (next t n a) (out t n a)
    where
        stateOutput Nothing _ = Nothing
        stateOutput (Just state) Nothing = Just (state, "")
        stateOutput (Just state) (Just output) = Just (state, output)

-- | same as `out`, but returns empty string when there's no output
outEmpty :: Trans -> Int -> Char -> String
outEmpty t n a = extractOutput $ out t n a
    where
        extractOutput Nothing   = ""
        extractOutput (Just o)  = o

-- | returns the output when performing a transition with the given symbol
out :: Trans -> Int -> Char -> Maybe String
out t n a = HashMap.lookup a (output $ state t n)

-- | returns the next state when performing a transition with the given symbol
next :: Trans -> Int -> Char -> Maybe Int
next t n a = HashMap.lookup a (transition $ state t n)

-- | returns the state with the given id
state :: Trans -> Int -> State
state (Trans {states}) n = states HashMap.! n

-- | updates the equivalence set to reflect the actual state, discarding the old one
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
    show = (foldr (++) "") . (map (++ "\n")) . ((curry showStateLines) (-1))

showTransLines :: Trans -> [String]
showTransLines t
    =  (showTransDataLines t)
    ++ (showVerifyEquivLines t)

showTransDataLines :: Trans -> [String]
showTransDataLines Trans {states, start}
    =  ["transducer with start " ++ (show start)]
    ++ (map ("  " ++) (foldr (++) [] (map showStateLines (HashMap.toList states))))

showStateLines :: (Int, State) -> [String]
showStateLines (n, State {transition, final, output})
    =  ["state " ++ (show n) ++ ":"]
    ++ ["  transitions:"]
    ++ (map ("    " ++) (showMapLines transition))
    ++ ["  outputs:"]
    ++ (map ("    " ++) (showMapLines output))
    ++ (showFinalLines final)

showVerifyEquivLines :: Trans -> [String]
showVerifyEquivLines t
    | verifyEquiv t = []
    | otherwise     = ["  WARN: transducer has wrong equivalence set"]

verifyEquiv :: Trans -> Bool
verifyEquiv Trans {states, equiv} = (HashSet.fromList (HashMap.elems states)) == equiv

showMapLines :: (Show k, Show v) => HashMap k v -> [String]
showMapLines = (map showPair) . HashMap.toList
    where
        showPair (k, v) = (show k) ++ " -> " ++ (show v)

showFinalLines :: (Show o) => Maybe o -> [String]
showFinalLines Nothing = []
showFinalLines (Just output) = ["  final with output " ++ (show output)]


instance Hashable State where
    hashWithSalt salt State {transition, final, output}
        = hashWithSalt salt (transition, final, output)

-- **** Data ****

data State = State {
    transition :: HashMap Char Int,
    final :: Maybe String,
    output :: HashMap Char String
} deriving (Eq)

data Trans = Trans {
    states :: HashMap Int State,
    equiv :: HashSet State,
    start :: Int
} deriving (Eq)

-- **** Utils ****

maybeTuple :: (Maybe a) -> (Maybe b) -> Maybe (a, b)
maybeTuple Nothing _ = Nothing
maybeTuple _ Nothing = Nothing
maybeTuple (Just x) (Just y) = Just (x, y)

emptyTrans :: Trans
emptyTrans = updateEquiv $ Trans {
    start = 1,
    states = HashMap.fromList [
        (1, State (HashMap.empty) Nothing (HashMap.empty))
    ],
    equiv = HashSet.fromList []
}

-- **** Sandbox ****


juljul :: Trans
juljul = updateEquiv $ Trans {
    start = 0,
    states = HashMap.fromList [
        (0, State 
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
                (HashMap.fromList [
                    ('r', 1)
                ])
                Nothing
                (HashMap.fromList [
                ])
        ),
        (3, State 
                (HashMap.fromList [
                    ('g', 1)
                ])
                Nothing
                (HashMap.fromList [
                ])
        ),
        (6, State 
                (HashMap.fromList [
                    ('e', 4)
                ])
                Nothing
                (HashMap.fromList [
                ])
        ),
        (4, State 
                (HashMap.fromList [
                    ('c', 1)
                ])
                Nothing
                (HashMap.fromList [
                ])
        ),
        (13, State 
                (HashMap.fromList [
                    ('e', 12)
                ])
                Nothing
                (HashMap.fromList [
                ])
        ),
        (12, State 
                (HashMap.fromList [
                    ('b', 8)
                ])
                Nothing
                (HashMap.fromList [
                ])
        ),
        (11, State 
                (HashMap.fromList [
                    ('a', 9)
                ])
                Nothing
                (HashMap.fromList [
                ])
        ),
        (9, State 
                (HashMap.fromList [
                    ('n', 1)
                ])
                Nothing
                (HashMap.fromList [
                ])
        ),
        (1, State 
                (HashMap.fromList [
                ])
                (Just "")
                (HashMap.fromList [
                ])
        ),
        (8, State 
                (HashMap.fromList [
                ])
                (Just "[8|9]")
                (HashMap.fromList [
                ])
        )
    ],
    equiv = HashSet.fromList []
}


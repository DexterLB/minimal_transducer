{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Transducer where

import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Data.Hashable (hashWithSalt, Hashable)

import Data.Maybe (fromJust)

import Data.List (zip4)

import Data.Text (Text)
import qualified Data.Text as T

import Data.Vector (Vector)
import qualified Data.Vector as V

import qualified Data.Vector.Algorithms.Search as VS

import Debug.Trace (trace)

-- **** traversing ****

-- | returns the output of the transducer for a given word
match :: Trans -> Text -> Maybe Text
match t w = makeOutput $ nextO' t (start t) w
    where
        makeOutput Nothing              = Nothing
        makeOutput (Just (n, sofar))    = finalOutput (final $ state t n) sofar

        finalOutput Nothing _           = Nothing
        finalOutput (Just suffix) sofar = Just $ sofar `T.append` suffix

-- | returns the state and output acquired by traversing with the given word
nextO' :: Trans -> Int -> Text -> Maybe (Int, Text)
nextO' t n w
    | length wordPath /= T.length w + 1 = Nothing
    | otherwise = Just (
            last wordPath,
            T.concat $ zipWith (outEmpty t) wordPath (T.unpack w)
        )
    where
        wordPath = path t n w

-- | returns the state acquired by making transitions with the given word
next' :: Trans -> Int -> Text -> Maybe Int
next' t n w
    | length wordPath == T.length w + 1     = Just (last wordPath)
    | otherwise                             = Nothing
    where
        wordPath = path t n w

-- | returns the path for traversing with the given word
path :: Trans -> Int -> Text -> [Int]
path _ n ""     = [n]
path t n word  = makePath (next t n a)
    where
        makePath Nothing = []
        makePath (Just stateID) = n : (path t stateID w)

        w = T.tail word
        a = T.head word

-- | returns the next state and output when performing a transition with the given symbol
nextO :: Trans -> Int -> Char -> Maybe (Int, Text)
nextO t n a = stateOutput (next t n a) (out t n a)
    where
        stateOutput Nothing _ = Nothing
        stateOutput (Just state) Nothing = Just (state, "")
        stateOutput (Just state) (Just output) = Just (state, output)

lastOutput :: Trans -> Int -> Text
lastOutput t n = V.last $ output (state t n)

-- | same as `out`, but returns empty string when there's no output
outEmpty :: Trans -> Int -> Char -> Text
outEmpty t n a = extractOutput $ out t n a
    where
        extractOutput Nothing   = ""
        extractOutput (Just o)  = o

-- | returns the output when performing a transition with the given symbol
out :: Trans -> Int -> Char -> Maybe Text
out t n a = ((output curState) V.!?) =<< (transitionIndex curState a)
    where
        curState = state t n

-- | returns the next state when performing a transition with the given symbol
next :: Trans -> Int -> Char -> Maybe Int
next t n a = ((transitionTo curState) V.!?) =<< (transitionIndex curState a)
    where
        curState = state t n

-- | returns the state with the given id
state :: Trans -> Int -> State
state (Trans {states}) n = states HashMap.! n

-- | updates the equivalence set to reflect the actual state, discarding the old one
updateEquiv :: Trans -> Trans
updateEquiv Trans {states, start, lastState} = Trans {
    states = states,
    start = start,
    equiv = Map.fromList $ map (\(x, y) -> (y, x)) $ HashMap.toList states,
    lastState = lastState
}

-- **** Mutations ****

-- | adds a transition, assuming it is lexicographically greater than the
-- | previous.
addTransitionLex :: Int -> Char -> Int -> Trans -> Trans
addTransitionLex from a to t = updateState t from f
    where
        f state 
            | (V.length tChars == 0) || a > (V.last tChars) = state {
                    transitionChar  = V.snoc tChars a,
                    transitionTo    = V.snoc tTo to,
                    output          = V.snoc out ""
                }
            | a == (V.last tChars) = state {
                    transitionTo = tTo V.//
                        [(last, to)]
                }
            | otherwise = error (
                    "trying to add an unordered transition to state "
                    ++ (show state) ++ ": " ++ (show (from, a, to))
                )
            where
                last = V.length tChars - 1
                tChars   = transitionChar state
                tTo      = transitionTo state
                out     = output state

bump :: (Eq k, Hashable k) => k -> v -> HashMap k v -> HashMap k v
bump key newValue m
    | HashMap.member key m  = m
    | otherwise             = HashMap.insert key newValue m

delState :: Int -> Trans -> Trans
delState n t = t {
        states = HashMap.delete n (states t)
--        equiv = HashMap.delete ((states t) HashMap.! n) (equiv t)
    }


prependToOutputs :: Trans -> Int -> Text -> Trans
prependToOutputs t n out = updateState t n f
    where
        f state = state {
            output = V.map (out `T.append`) (output state),
            final = (out `T.append`) <$> (final state)
        }


setFinal :: Trans -> Int -> Text -> Trans
setFinal t n out = updateState t n f
    where
        f state = state {
            final = Just out
        }

setOutput :: Trans -> Int -> Char -> Text -> Trans
setOutput t n a out = updateState t n f
    where
        f state = state {
                output = (output state) V.// [(index, out)]
            }
            where
                index = fromJust $ transitionIndex state a

setLastOutput :: Trans -> Int -> Char -> Text -> Trans
setLastOutput t n a out = updateState t n f
    where
        f state = state {
                output = (output state) V.// [(index, out)]
            }
            where
                index
                    | V.last (transitionChar state) == a 
                        = V.length (transitionChar state) - 1
                    | otherwise = error "setting non-last output"

setOutputAt :: Trans -> Int -> Int -> Text -> Trans
setOutputAt t n index out = updateState t n f
    where
        f state = state {
            output = (output state) V.// [(index, out)]
        }

-- | updates a state both in the state table
updateState :: Trans -> Int -> (State -> State) -> Trans
updateState t n f = t {
            states = HashMap.adjust f n (states t)
        }
    -- equiv doesn't need updating because updateState is only called
    -- before inserting a state in equiv


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
showStateLines (n, State {
        transitionChar, transitionTo, final, output
    })
        =  ["state " ++ (show n) ++ ":"]
        ++ ["  transitions:"]
        ++ (map (("    " ++) . showTransition) transitions)
        ++ (showFinalLines final)
    where
        transitions = (zip4
            (repeat n)
            (V.toList transitionChar)
            (V.toList transitionTo)
            (V.toList output))

showVerifyEquivLines :: Trans -> [String]
showVerifyEquivLines t
    | verifyEquiv t = []
    | otherwise     = ["  WARN: transducer has wrong equivalence set"]

verifyEquiv :: Trans -> Bool
verifyEquiv Trans {states, equiv} = (
        Map.fromList $ map (\(x, y) -> (y, x)) $ (HashMap.toList states)
    ) == equiv

showTransition :: (Int, Char, Int, Text) -> String
showTransition (from, c, to, output) = 
    (show from) ++ " [" ++ [c] ++ "] -> " ++ (show to) ++ ": " ++ (T.unpack output)

showMapLines :: (Show k, Show v) => HashMap k v -> [String]
showMapLines = showPairLines . HashMap.toList

showPairLines :: (Show k, Show v) => [(k, v)] -> [String]
showPairLines = map showPair

showPair :: (Show k, Show v) => (k, v) -> String
showPair (k, v) = (show k) ++ " -> " ++ (show v)

showFinalLines :: (Show o) => Maybe o -> [String]
showFinalLines Nothing = []
showFinalLines (Just output) = ["  final with output " ++ (show output)]

-- to do: binary search
transitionIndex :: State -> Char -> Maybe Int
transitionIndex (State {transitionChar}) c = V.elemIndex c transitionChar

instance Hashable State where
    hashWithSalt salt State {transitionChar, transitionTo, final, output}
        -- it appears that most of the time it's faster to hash only the size
        -- of the output and whether the state is final, and manually compare
        -- the remaining states for equivalence
        --
        -- sometimes, on a particular worst-case input, the other option works
        -- better.


        -- = hashWithSalt salt (transition, T.length <$> final, HashMap.size output)
        = hashWithSalt salt (
            V.toList transitionChar, 
            V.toList transitionTo, 
            final, 
            V.toList output)

instance Ord State where
    compare a b
        | a_is_final && (not b_is_final)    = LT
        | (not a_is_final) && b_is_final    = GT
        | destinations /= EQ                = destinations
        | labels /= EQ                      = labels
        | outputs /= EQ                     = outputs
        | final_output /= EQ                = final_output
        | otherwise                         = EQ
        where
            a_is_final = final a /= Nothing
            b_is_final = final b /= Nothing

            labels = compare (transitionChar a) (transitionChar b)
            destinations = compare (transitionTo a) (transitionTo b)
            outputs = compare (output a) (output b)
            final_output = compare (final a) (final b)

-- **** Data ****

data State = State {
    transitionChar :: Vector Char,
    transitionTo :: Vector Int,
    final :: Maybe Text,
    output :: Vector Text
} deriving (Eq)

data Trans = Trans {
    states :: HashMap Int State,
    equiv :: Map State Int,
    start :: Int,
    lastState :: Int
} deriving (Eq)

-- **** Utils ****

maybeTuple :: (Maybe a) -> (Maybe b) -> Maybe (a, b)
maybeTuple Nothing _ = Nothing
maybeTuple _ Nothing = Nothing
maybeTuple (Just x) (Just y) = Just (x, y)

emptyTrans :: Trans
emptyTrans = Trans {
    start = 1,
    states = HashMap.fromList [
        (1, State (V.empty) (V.empty) Nothing (V.empty))
    ],
    equiv = Map.empty,
    lastState = 1
}

emptyState :: State
emptyState = State {
    transitionChar = V.empty,
    transitionTo = V.empty,
    output = V.empty,
    final = Nothing
}

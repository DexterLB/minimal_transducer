{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Transducer where

import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

import Data.Hashable (hashWithSalt, Hashable)

import Data.Text (Text)
import qualified Data.Text as T

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

-- | same as `out`, but returns empty string when there's no output
outEmpty :: Trans -> Int -> Char -> Text
outEmpty t n a = extractOutput $ out t n a
    where
        extractOutput Nothing   = ""
        extractOutput (Just o)  = o

-- | returns the output when performing a transition with the given symbol
out :: Trans -> Int -> Char -> Maybe Text
out t n a = HashMap.lookup a (output $ state t n)

-- | returns the next state when performing a transition with the given symbol
next :: Trans -> Int -> Char -> Maybe Int
next t n a = HashMap.lookup a (transition $ state t n)

-- | returns the state with the given id
state :: Trans -> Int -> State
state (Trans {states}) n = states HashMap.! n

-- | updates the equivalence set to reflect the actual state, discarding the old one
updateEquiv :: Trans -> Trans
updateEquiv Trans {states, start, lastState} = Trans {
    states = states,
    start = start,
    equiv = HashMap.fromList $ map (\(x, y) -> (y, x)) $  HashMap.toList states,
    lastState = lastState
}

-- **** Mutations ****

addTransition :: Int -> Char -> Int -> Trans -> Trans
addTransition from a to t = updateState t from f
    where
        f state = state {
                transition = HashMap.insert a to (transition state),
                output = bump a "" (output state)
            }

bump :: (Eq k, Hashable k) => k -> v -> HashMap k v -> HashMap k v
bump key newValue m
    | HashMap.member key m  = m
    | otherwise             = HashMap.insert key newValue m

delState :: Int -> Trans -> Trans
delState n t = t {
        states = HashMap.delete n (states t)
--        equiv = HashMap.delete ((states t) HashMap.! n) (equiv t)
--        we'll only be deleting states that aren't in equivalence
    }


prependToOutputs :: Trans -> Int -> Text -> Trans
prependToOutputs t n out = updateState t n f
    where
        f state = state {
            output = HashMap.map (out `T.append`) (output state),
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
            output = HashMap.insert a out (output state)
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
verifyEquiv Trans {states, equiv} = (
        HashMap.fromList $ map (\(x, y) -> (y, x)) $ (HashMap.toList states)
    ) == equiv

showMapLines :: (Show k, Show v) => HashMap k v -> [String]
showMapLines = (map showPair) . HashMap.toList
    where
        showPair (k, v) = (show k) ++ " -> " ++ (show v)

showFinalLines :: (Show o) => Maybe o -> [String]
showFinalLines Nothing = []
showFinalLines (Just output) = ["  final with output " ++ (show output)]

transitionCount :: Trans -> Int
transitionCount Trans {states} = sum $ HashMap.map (\s -> HashMap.size $ transition s) states


instance Hashable State where
    hashWithSalt salt State {transition, final, output}
        -- it appears that most of the time it's faster to hash only the size
        -- of the output and whether the state is final, and manually compare
        -- the remaining states for equivalence
        --
        -- sometimes, on a particular worst-case input, the other option works
        -- better.


        -- = hashWithSalt salt (transition, T.length <$> final, HashMap.size output)
        = hashWithSalt salt (transition, final, output)
    

-- **** Data ****

data State = State {
    transition :: HashMap Char Int,
    final :: Maybe Text,
    output :: HashMap Char Text
} deriving (Eq)

data Trans = Trans {
    states :: HashMap Int State,
    equiv :: HashMap State Int,
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
        (1, State (HashMap.empty) Nothing (HashMap.empty))
    ],
    equiv = HashMap.fromList [],
    lastState = 1
}

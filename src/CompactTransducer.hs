{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module CompactTransducer where

import Data.Vector (Vector, (!))
import qualified Data.Vector as V

import Data.Text (Text)
import qualified Data.Text as T

data Trans = Trans {
    nStates             :: Int,

    -- these arrays are indexed with the state ID
    
    -- for each state, contains an index in the transitionSources array
    froms               :: Vector Int,

    -- this is a pseudo-2d vector with n = nStates.
    -- for each state, for each transition, contains a state ID
    tos                 :: Vector Int,

    -- this is a pseudo-2d vector with n = nStates.
    -- for each state, for each transition, contains an index in the
    -- possibleOutputs array
    outputs             :: Vector Int,

    -- for each state, contains an index in the possibleOutputs array
    -- if the state is final, or -1 if it's not
    finals              :: Vector Int,

    -- these arrays are indexed on their own

    -- this array contains all possible sets of outbound labels
    transitionSources   :: Vector Text,

    -- this array contains all possible outputs
    possibleOutputs     :: Vector Text
}

-- | returns the result from traversing the transducer with the given word
match :: Trans -> Text -> Maybe Text
match t word
    | length path /= T.length word = Nothing
    | otherwise = T.concat <$> (\x -> (map snd path) ++ [x]) <$> finalOutput t (fst $ last path)
    where
        path = pathO t 0 word

-- | returns the output of the given state if it's final
finalOutput :: Trans -> Int -> Maybe Text
finalOutput t state = check (finals t ! state)
    where
        check :: Int -> Maybe Text
        check -1 = Nothing
        check outputIndex = Just $ (possibleOutputs t) ! outputIndex

-- | traverses from the given state with the given word, and returns the
-- | result path with respective outputs
pathO :: Trans -> Int -> Text -> [(Int, Text)]
pathO t state word = reverse $ pathO' [] t state word
    where
        pathO' :: [(Int, Text)] -> Trans -> Int -> Text -> [(Int, Text)]
        pathO' accum _ _ "" = accum
        pathO' accum t state word = check $ nextO t state char
            where
                check :: Maybe (Int, Text) -> [(Int, Text)]
                check Nothing = accum
                check (Just (nextState, output)) 
                    = pathO' ((nextState, output) : accum) t nextState rest

                rest = T.tail word
                char = T.head word

-- | returns the next state and output with the given transition
nextO :: Trans -> Int -> Char -> Maybe (Int, Text)
nextO t state c = (transition t state) <$> (lookupLabel t state c)

-- | returns the next state and output acquired from a transition
-- | from the given state and label index
transition :: Trans -> Int -> Int -> (Int, Text)
transition t stateID labelID = (next, out)
    where
        next =                        l2 t tos     stateID labelID
        out  = (possibleOutputs t) ! (l2 t outputs stateID labelID)

-- | returns the index of the transition from the given state with
-- | the given label
lookupLabel :: Trans -> Int -> Char -> Maybe Int
lookupLabel t id c = T.findIndex (== c) labels
    where
        labels = ((transitionSources t) ! (froms t ! id))

-- | performs a lookup in one of the pseudo-2D arrays
l2 :: Trans -> (Trans -> Vector a) -> Int -> Int -> a
l2 t field i j = array ! (i * (nStates t) + j)
    where
        array = field t
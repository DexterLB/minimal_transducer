{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module JsonTransducer where

import GHC.Generics

import Data.Vector (Vector, (!))
import qualified Data.Vector as V

import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

import Data.Aeson (ToJSON, FromJSON)

-- | returns the result from traversing the transducer with the given word
match :: Trans -> Text -> Maybe Text
match t word
    | length path /= T.length word = Nothing
    | otherwise = T.concat <$> (\x -> (map snd path) ++ [x]) <$> finalOutput t (fst $ last path)
    where
        path = pathO t 0 word

-- | traverses from the given state with the given word, and returns the
-- | result path with respective outputs
pathO :: Trans -> Int -> Text -> [(Int, Text)]
pathO t state word = reverse $ pathO' [] t state word
    where
        pathO' :: [(Int, Text)] -> Trans -> Int -> Text -> [(Int, Text)]
        pathO' accum _ _ "" = accum
        pathO' accum t' state' word' = check $ nextO t' state' char
            where
                check :: Maybe (Int, Text) -> [(Int, Text)]
                check Nothing = accum
                check (Just (nextState, output)) 
                    = pathO' ((nextState, output) : accum) t' nextState rest

                rest = T.tail word'
                char = T.head word'

-- | returns the next state and output with the given transition
nextO :: Trans -> Int -> Char -> Maybe (Int, Text)
nextO t stateID c = (tuplify t) <$> target
    where
        target = (\l -> HashMap.lookup l (transitions state)) =<< (lookupLabel t c)
        state = (states t) ! stateID

-- | returns the output of the given state if it's final
finalOutput :: Trans -> Int -> Maybe Text
finalOutput t stateID = check $ final $ (states t) ! stateID
    where
        check :: Int -> Maybe Text
        check outputIndex
            | outputIndex < 0 = Nothing
            | otherwise       = Just $ (possibleOutputs t) ! outputIndex

data Trans = Trans {
    states          :: Vector State,
    alphabet        :: Text,
    possibleOutputs :: Vector Text
} deriving (Generic)

instance FromJSON Trans
instance   ToJSON Trans

data State = State {
    transitions :: HashMap Int Target,
    final   :: Int  -- ^ -1 if not final or index in possibleOutputs
} deriving (Generic)

instance FromJSON State
instance   ToJSON State

data Target = Target {
    state   :: Int, -- ^ index in the states array
    output  :: Int  -- ^ index in the possibleOutputs array
} deriving (Generic)

tuplify :: Trans -> Target -> (Int, Text)
tuplify t (Target {state, output})
    = (state, (possibleOutputs t) ! output)

lookupLabel :: Trans -> Char -> Maybe Int
lookupLabel t c = T.findIndex (== c) $ alphabet t

instance FromJSON Target
instance   ToJSON Target
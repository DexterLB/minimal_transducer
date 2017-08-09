{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module JsonTransducer where

import GHC.Generics

import Data.Vector (Vector, (!))
import qualified Data.Vector as V

import Data.Text (Text)
import qualified Data.Text as Text

import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

import Data.Aeson (ToJSON, FromJSON)

data Trans = Trans {
    states          :: Vector State,
    alphabet        :: Vector Char,
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

instance FromJSON Target
instance   ToJSON Target
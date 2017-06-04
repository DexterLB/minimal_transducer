{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Minimal where

import Transducer

import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

addWord :: Trans -> String -> String -> Trans
addWord t prevWord word = newT
    where
        -- first minimise the prevword suffix
        --

        (newT, path) = makePath t word
        rest = drop (length prefix) word 
        prefix = lcp prevWord word

-- ^ traverse from the start with the given word, making states as needed
makePath :: Trans -> String -> (Trans, [Int])
makePath t w = makePathFrom t (start t) w

-- ^ traverse from the given state with the given word, making states as needed
makePathFrom :: Trans -> Int -> String -> (Trans, [Int])
makePathFrom t n "" = (t, [n])
makePathFrom t n (a:w) = f (next t n a)
    where   -- todo: use tail recursion here
        f :: Maybe Int -> (Trans, [Int])
        f (Just m) = (newT, m : newPath)
            where
                (newT, newPath) = makePathFrom t m w
        f Nothing = (newT, m: newPath)
            where
                (newT, newPath) = makePathFrom tt m w
                (tt, m) = addState t n a

-- ^ adds a new state after the given state with the given transition
addState :: Trans 
         -> Int             -- % the state from which we make a transition
         -> Char            -- % with which symbol
         -> (Trans, Int)
addState (Trans {start, states, equiv}) prevStateID a 
    = (Trans {start = start', states = states', equiv = equiv'}, newStateID)
        where
            start' = start

            states' = addTransition prevStateID a newStateID $ HashMap.insert newStateID newState states

            equiv' = equiv

            newState = State {
                transition = HashMap.fromList [],
                final = Nothing,
                output = HashMap.fromList []
            }

            newStateID = (HashMap.size states) + 1 -- fixme

-- ^ adds a transition in a state table
addTransition :: Int -> Char -> Int -> HashMap Int State -> HashMap Int State
addTransition from a to states = HashMap.insert from newState states
    where
        newState = state {
            transition = HashMap.insert a to (transition state)
        }
        state = states HashMap.! from

-- **** utils ****

-- ^ longest common prefix
lcp :: String -> String -> String
lcp (x:xs) (y:ys)
    | x == y        = x : (lcp xs ys)
    | otherwise     = ""
lcp _ _             = ""

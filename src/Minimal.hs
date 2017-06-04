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

minimisePath :: Trans -> [(Int, Char, Int)] -> Trans
minimisePath t = foldr minimiseTransition t

-- | traverse from the start with the given word, making states as needed
makePath :: Trans -> String -> (Trans, [Int])
makePath t w = makePathFrom t (start t) w

-- | traverse from the given state with the given word, making states as needed
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

-- | adds a new state after the given state with the given transition
addState :: Trans 
         -> Int             -- ^ the state from which we make a transition
         -> Char            -- ^ with which symbol
         -> (Trans, Int)
addState t prevStateID a 
    = (t', newStateID)
        where
            t' = addTransition prevStateID a newStateID $ t {
                states = HashMap.insert newStateID newState (states t),
                lastState = newStateID
            }

            newState = State {
                transition = HashMap.fromList [],
                final = Nothing,
                output = HashMap.fromList []
            }

            newStateID = lastState t + 1

minimiseTransition :: (Int, Char, Int) -> Trans -> Trans
minimiseTransition (from, a, to) t = checkEquiv toEquiv
    where
        checkEquiv Nothing = t  -- state is unique
        checkEquiv (Just n)
            | n == to   = undefined   -- state is equivalent to itself - this shouldn't happen
            | otherwise = (addTransition from a n $ delState to t) {
                    equiv = HashMap.insert (state t n) n (equiv t)
                }

        toEquiv = HashMap.lookup ((states t) HashMap.! to) (equiv t)

addTransition :: Int -> Char -> Int -> Trans -> Trans
addTransition from a to t = t {
        states = addTransitionState from a to (states t)
    }


-- | adds a transition in a state table
addTransitionState :: Int -> Char -> Int -> HashMap Int State -> HashMap Int State
addTransitionState from a to states = HashMap.insert from newState states
    where
        newState = state {
            transition = HashMap.insert a to (transition state)
        }
        state = states HashMap.! from

delState :: Int -> Trans -> Trans
delState n t = t {
        states = HashMap.delete n (states t)
    }

-- **** utils ****

-- | longest common prefix
lcp :: String -> String -> String
lcp (x:xs) (y:ys)
    | x == y        = x : (lcp xs ys)
    | otherwise     = ""
lcp _ _             = ""

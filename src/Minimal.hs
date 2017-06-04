{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Minimal where

import Transducer

import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

addWord :: Trans -> String -> String -> String -> Trans
addWord t prevWord word output = outT
    where
        (outT, leftoverOutput) = addOutput newT prefixPath prefix output
        
        (newT, newPath) = makePath minT word

        -- minimise the suffix of the previous word because it diverges
        -- with the current word
        minT = minimisePath t prevSuffix prevSuffixPath

        suffix = drop (length prefix) word
        prevSuffixPath = drop (length prefix) (path t (start t) prevWord)
        prevSuffix = drop (length prefix) prevWord
        prefixPath = path t (start t) prefix
        prefix = lcp prevWord word

addOutput :: Trans -> [Int] -> String -> String -> (Trans, String)
addOutput t _ "" output = (t, output)
addOutput t (p:q:path) (a:word) output = addOutput newT (q:path) word suffix
    where
        -- move incompatible outputs to the right
        newT = prependToOutputs tWithOutputPrefix q currentSuffix

        -- output the common prefix at the current transition
        tWithOutputPrefix = setOutput t p a commonPrefix

        currentSuffix = drop (length commonPrefix) currentOutput
        suffix = drop (length commonPrefix) output
        commonPrefix = lcp currentOutput output
        currentOutput = outEmpty t p a

-- | if the given transducer is minimal except for (pref . word), it now becomes
-- | minimal except for pref.
minimisePath :: Trans -> String -> [Int] -> Trans
minimisePath t word path = minimiseZippedPath t zipped
    where
        zipped = zipWith (\(x, y) z -> (x, y, z)) (zip path word) (tail path)

minimiseZippedPath :: Trans -> [(Int, Char, Int)] -> Trans
minimiseZippedPath t = foldr minimiseTransition t

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

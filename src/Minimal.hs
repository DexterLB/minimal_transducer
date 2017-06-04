{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Minimal where

import Transducer

import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

import Data.Foldable (foldl')

-- | perform final minimisation of a transducer minimal except for word
finalise :: (Trans, String) -> Trans
finalise (t, word) = minimiseWord t word

-- | equivalent to multiple calls of addWordI. Works well with huge lazy lists.
addWords :: (Trans, String) -> [(String, String)] -> (Trans, String)
addWords = foldl' addWordI

-- | the "transformation" version of addWord. The (transducer, lastWord) tuple
-- | is transformed, adding the new word.
addWordI :: (Trans, String)     -- ^ transducer, lastWord
         -> (String, String)    -- ^ newWord, output
         -> (Trans, String)     -- ^ newTransducer, newWord
addWordI (t, prevWord) (newWord, output)
    = (addWord t prevWord newWord output, newWord)

-- | add a word to the transducer. It must be minimal except for the previous word,
-- | and after this operation will be minimal except for the new word.
addWord :: Trans
        -> String   -- ^ previous word
        -> String   -- ^ word to add (must be lexicographically > than previous)
        -> String   -- ^ output for the added word
        -> Trans
addWord t prevWord word output = finalT
    where
        -- set the remaining output to the first transition that's only part
        -- of the new word (e.g. the first element of the suffix)
        finalT = setOutput tWithOut (head suffixPath) (head suffix) leftoverOutput

        -- set any outputs we have in common with parts of the prefix
        (tWithOut, leftoverOutput) = addOutput newT prefixPath prefix output
       
        suffixPath = drop (length prefix) newPath

        -- set the last state of the current word's path to final (with no output)
        newT = setFinal tWithNewStates (last newPath) ""

        -- generate a path for the new word in the transducer
        (tWithNewStates, newPath) = makePath minT word

        -- minimise the suffix of the previous word because it diverges
        -- with the current word
        minT = minimisePath t prevSuffix prevSuffixPath

        -- the "suffix" is the part of the new word that has no common
        -- prefix with any previous word
        suffix = drop (length prefix) word

        -- the "prev suffix" is the suffix of the previous word which diverges
        -- from the new word. It will be minimised and forgotten about.
        prevSuffixPath = drop (length prefix) (path t (start t) prevWord)
        prevSuffix = drop (length prefix) prevWord

        -- the "prefix" is the common prefix of the current and previous words
        prefixPath = path t (start t) prefix
        prefix = lcp prevWord word

-- | attach the given output to the word with the given path
addOutput :: Trans
          -> [Int]              -- ^ path
          -> String             -- ^ word (must be compatible with the path)
          -> String             -- ^ output
          -> (Trans, String)    -- ^ new transducer and the leftover output
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

-- | if the transducer is minimal except for word, it now becomes minimal.
minimiseWord :: Trans -> String -> Trans
minimiseWord t w = minimisePath t w (path t (start t) w)

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
        f (Just m) = (newT, n : newPath)
            where
                (newT, newPath) = makePathFrom t m w
        f Nothing = (newT, n : newPath)
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

-- | checks if there's a state which is equivalent to the target state.
-- | If there is, the target state is deleted and the transition is pointed
-- | at its equivalent state
minimiseTransition :: (Int, Char, Int) -> Trans -> Trans
minimiseTransition (from, a, to) t = checkEquiv toEquiv
    where
        checkEquiv Nothing = t {  -- state is unique, add it to the equivalence table
                equiv = HashMap.insert (state t to) to (equiv t)
            }
        checkEquiv (Just n)
            | n == to   = undefined   -- state is equivalent to itself - this shouldn't happen
            | otherwise = (addTransition from a n $ delState to t) {
                    equiv = HashMap.insert (state t n) n (equiv t)
                }

        toEquiv = HashMap.lookup ((states t) HashMap.! to) (equiv t)

-- **** utils ****

-- | longest common prefix
lcp :: String -> String -> String
lcp (x:xs) (y:ys)
    | x == y        = x : (lcp xs ys)
    | otherwise     = ""
lcp _ _             = ""

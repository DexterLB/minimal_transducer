{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}

module Minimal where

import Transducer

import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

import Data.Foldable (foldl')

import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace (trace)

type Except = (Text, [Int]) -- word and path for which the transducer is not minimal

-- | constructs a minimal transducer with the given dictionary.
-- | Keys must be sorted.
minimalTransducer :: [(Text, Text)] -> Trans
minimalTransducer inp = finalise $ addWords (emptyTrans, ("", [])) inp

-- | perform final minimisation of a transducer minimal except for word
finalise :: (Trans, Except) -> Trans
finalise (t, (word, path)) = minimisePath t word path

-- | equivalent to multiple calls of addWordI. Works well with huge lazy lists.
-- | Keys must be sorted.
addWords :: (Trans, Except) -> [(Text, Text)] -> (Trans, Except)
addWords = foldl' addWordI

-- | add a word to the transducer. It must be minimal except for the previous word,
-- | and after this operation will be minimal except for the new word.
addWordI :: (Trans, Except)       -- ^ transducer, lastWord, lastPath
         -> (Text, Text)          -- ^ newWord, output
         -> (Trans, Except)       -- ^ newTransducer, newWord, newPath
addWordI (!t, (!prevWord, !prevPath)) (!word, !output)
    | T.length suffix /= 0 = (finalT, (word, newPath))
    | otherwise = error $ 
                          "words " 
                          ++ (T.unpack prevWord) 
                          ++ " and " 
                          ++ (T.unpack word)
                          ++ " are out of order"
    where
        -- set the remaining output to the first transition that's only part
        -- of the new word (e.g. the first element of the suffix)
        finalT = setLastOutput tWithOut (head suffixPath) (T.head suffix) leftoverOutput

        -- set any outputs we have in common with parts of the prefix
        (tWithOut, leftoverOutput) = addOutput newT prefixPath prefix output
       
        suffixPath = drop (T.length prefix) newPath

        -- set the last state of the current word's path to final (with no output)
        newT = setFinal tWithNewStates (last newPath) ""

        -- generate a path for the new word in the transducer
        (tWithNewStates, newPath) = makePath minT word

        -- minimise the suffix of the previous word because it diverges
        -- with the current word
        minT = minimisePath t prevSuffix prevSuffixPath


        -- the "prefix" is the common prefix of the current and previous words
        
        -- the "suffix" is the part of the new word that has no common
        -- prefix with any previous word
        --
        -- the "prev suffix" is the suffix of the previous word which diverges
        -- from the new word. It will be minimised and forgotten about.
        prevSuffixPath                  = drop  prefixLength         prevPath
        prefixPath                      = take (prefixLength + 1)    prevPath

        prefixLength                    = T.length prefix
        (prefix, prevSuffix, suffix)    = lcprefixes prevWord word

-- | attach the given output to the word with the given path
addOutput :: Trans
          -> [Int]            -- ^ path
          -> Text             -- ^ word (must be compatible with the path)
          -> Text             -- ^ output
          -> (Trans, Text)    -- ^ new transducer and the leftover output
addOutput t _ "" output = (t, output)
addOutput t (p:q:path) word output = addOutput newT (q:path) w suffix
    where
        -- move incompatible outputs to the right
        newT = prependToOutputs tWithOutputPrefix q currentSuffix

        -- output the common prefix at the current transition
        tWithOutputPrefix = setLastOutput t p a commonPrefix

        (commonPrefix, currentSuffix, suffix) = lcprefixes currentOutput output
        
        currentOutput   = outEmpty t p a

        a = T.head word
        w = T.tail word

-- | if the transducer is minimal except for word, it now becomes minimal.
minimiseWord :: Trans -> Text -> Trans
minimiseWord t w = minimisePath t w (path t (start t) w)

-- | if the given transducer is minimal except for (pref . word), it now becomes
-- | minimal except for pref.
minimisePath :: Trans -> Text -> [Int] -> Trans
minimisePath t word path = minimiseZippedPath t zipped
    where
        zipped = zipWith (\(x, y) z -> (x, y, z)) (zip path (T.unpack word)) (tail path)

minimiseZippedPath :: Trans -> [(Int, Char, Int)] -> Trans
minimiseZippedPath t = foldr minimiseTransition t

-- | traverse from the start with the given word, making states as needed
makePath :: Trans -> Text -> (Trans, [Int])
makePath t w = makePathFrom t (start t) w

-- | traverse from the given state with the given word, making states as needed
makePathFrom :: Trans -> Int -> Text -> (Trans, [Int])
makePathFrom t n "" = (t, [n])
makePathFrom t n word = f (next t n a)
    where   -- todo: use tail recursion here
        f :: Maybe Int -> (Trans, [Int])
        f (Just m) = (newT, n : newPath)
            where
                (newT, newPath) = makePathFrom t m w
        f Nothing = (newT, n : newPath)
            where
                (newT, newPath) = makePathFrom tt m w
                (tt, m) = addState t n a

        a = T.head word
        w = T.tail word

-- | adds a new state after the given state with the given transition
addState :: Trans 
         -> Int             -- ^ the state from which we make a transition
         -> Char            -- ^ with which symbol
         -> (Trans, Int)
addState t prevStateID a 
    = (t', newStateID)
        where
            t' = addTransitionLex prevStateID a newStateID $ t {
                states = HashMap.insert newStateID newState (states t),
                lastState = newStateID
            }

            newState = emptyState

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
            | n == to   = t   -- state is already part of the minimal path
            | otherwise = t' {
                    equiv = HashMap.insert (state t n) n (equiv t')
                }
            where
                t' = (addTransitionLex from a n $ delState to t)

        toEquiv = HashMap.lookup ((states t) HashMap.! to) (equiv t)

-- **** utils ****

-- | longest common prefix
lcp :: Text -> Text -> Text
lcp a b = fst3 (lcprefixes a b)

lcprefixes :: Text -> Text -> (Text, Text, Text)
lcprefixes a b = couldBeEmpty (T.commonPrefixes a b)
    where
        couldBeEmpty Nothing            = (T.empty, a, b)
        couldBeEmpty (Just prefixes)    = prefixes

fst3 (a, _, _) = a

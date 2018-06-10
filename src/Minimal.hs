{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}

module Minimal where

import Transducer

import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict ()

import Data.Foldable (foldl')

import Data.Text (Text)
import qualified Data.Text as T

import Data.List (mapAccumR)
import Debug.Trace

type Except = (Text, [Int]) -- word and path for which the transducer is not minimal

-- | constructs a minimal transducer with the given dictionary.
-- | Keys must be sorted.
minimalTransducer :: [(Text, Text)] -> Trans
minimalTransducer inp = finalise $ addWords (emptyExcept emptyTrans) inp

-- | perform final minimisation of a transducer minimal except for word
finalise :: (Trans, Except) -> Trans
finalise (t, (word, wordPath)) = minimisePath t word wordPath

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
        finalT = setOutput newT (head suffixPath) (T.head suffix) leftoverOutput

        -- set the last state of the current word's path to final (with no output)
        newT = setFinal tWithOut (last newPath) ""

        -- set any outputs we have in common with parts of the prefix
        (tWithOut, leftoverOutput) = addOutput tWithNewStates prefixPath prefix output

        -- generate a path for the new word in the transducer
        newPath = prefixPath ++ (tail suffixPath)
        (tWithNewStates, suffixPath) = makePathAfter minT (last prefixPath) suffix

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

-- | Add a word to a minimal transducer. Words added this way may be out of order.
addWordU :: Trans           -- ^ transducer, lastWord, lastPath
         -> (Text, Text)    -- ^ newWord, output
         -> Trans           -- ^ newTransducer, newWord, newPath
addWordU !t (!word, !output)
    | T.length suffix /= 0 = finalise (finalT, (word, newPath))
    | otherwise = finalise (newTNoSuffix, (word, newPath))
    where
        -- set the remaining output to the first transition that's only part
        -- of the new word (e.g. the first element of the suffix)
        finalT = setOutput newT (head suffixPath) (T.head suffix) leftoverOutput

        -- set the last state of the current word's path to final (with no output)
        newT = setFinal tWithOut (last newPath) ""

        newTNoSuffix = setFinal tWithOut (last newPath) leftoverOutput

        -- set any outputs we have in common with parts of the prefix
        (tWithOut, leftoverOutput) = addOutput tWithNewStates prefixPath prefix output

        -- generate a path for the new word in the transducer
        newPath = prefixPath ++ (tail suffixPath)
        (tWithNewStates, suffixPath) = makePathAfter minT (last prefixPath) suffix

        (prefix, prefixPath) = (unzipPath (start t) zippedPrefixPath)
        (minT, zippedPrefixPath, suffix) = unminimisePrefix t word

delWordU :: Trans
         -> Text
         -> Trans
delWordU !t !word
    | suffix /= "" = t  -- word is longer than its prefix in the transducer
    | final (state minT lastState) == Nothing = t   -- last state is not final
    | otherwise = finalise (finalT, unzipPath (start finalT) leftPath)
    where
        -- move any free output prefixes backwards
        finalT = foldr pullOutputs newT leftPath

        -- make last state non-final and remove unused states
        (newT, leftPath) = trim (unFinal minT lastState) $ reverse prefixPath

        lastState = (\(_, _, s) -> s) $ last prefixPath

        -- make the transducer minimal except for the word
        (minT, prefixPath, suffix) = unminimisePrefix t word

-- | takes a REVERSED path and removes states until it reaches a final or
-- | divergent state. Returns the (non-reversed) path from the start to
-- | the last untouched state.
trim :: Trans -> [(Int, Char, Int)] -> (Trans, [(Int, Char, Int)])
trim t ((prev, a, n):rest)
    | final (state t n) == Nothing && HashMap.size (transition (state t n)) == 0
        = trim (delState n t') rest
    | otherwise = (t, reverse ((prev, a, n):rest))
    where
        t' = delTransition prev a n t
trim t [] = (t, [])

-- | pull the common output prefix of the target state into the output
-- | of the source state
pullOutputs :: (Int, Char, Int) -> Trans -> Trans
pullOutputs (from, a, to) t = appendToOutput t' from a commonPrefix
    where
        t' = removeOutputPrefix t to commonPrefix
        commonPrefix = commonOutputPrefix t to

-- | attach the given output to the word with the given path
addOutput :: Trans
          -> [Int]            -- ^ path
          -> Text             -- ^ word (must be compatible with the path)
          -> Text             -- ^ output
          -> (Trans, Text)    -- ^ new transducer and the leftover output
addOutput t _ "" output = (t, output)
addOutput t (p:q:wordPath) word output = addOutput newT (q:wordPath) w suffix
    where
        -- move incompatible outputs to the right
        newT = prependToOutputs tWithOutputPrefix q currentSuffix

        -- output the common prefix at the current transition
        tWithOutputPrefix = setOutput t p a commonPrefix

        (commonPrefix, currentSuffix, suffix) = lcprefixes currentOutput output

        currentOutput   = outEmpty t p a

        a = T.head word
        w = T.tail word

-- | if the transducer is minimal except for word, it now becomes minimal.
minimiseWord :: Trans -> Text -> Trans
minimiseWord t w = minimisePath t w (path t (start t) w)

-- | if the transducer is minimal, it becomes minimal except for the longest
-- | prefix of the given word that's in the transducer. Returns the resulting
-- | transducer, the path of the prefix and the leftover suffix.
unminimisePrefix :: Trans -> Text -> (Trans, [(Int, Char, Int)], Text)
unminimisePrefix t w = (newTrans, newPath , rest)
    where
        (newTrans, newPath) = unminimiseZippedPath t path
        (path, rest) = traverseZipped t w

-- | makes a minimal transducer minimal except for a path with the same word
-- | as the given.
-- | returns the resulting minimal-except path.
unminimiseZippedPath :: Trans -> [(Int, Char, Int)] -> (Trans, [(Int, Char, Int)])
unminimiseZippedPath t l = (t', reverse l')
    where
        (t', l') = unminimiseZippedPath' t l []

unminimiseZippedPath' :: Trans -> [(Int, Char, Int)] -> [(Int, Char, Int)] -> (Trans, [(Int, Char, Int)])
unminimiseZippedPath' t [] p = (t, p)
unminimiseZippedPath' t ((m, a, n):(_, b, q):path) p = unminimiseZippedPath' t' ((n', b, q):path) ((m, a, n'):p)
    where
        (t', n') = unminimiseTransition t (m, a, n)
unminimiseZippedPath' t [(m, a, n)] p = (t', ((m, a, n'):p))
    where
        (t', n') = unminimiseTransition t (m, a, n)


-- | if the given transducer is minimal except for (pref . word), it now becomes
-- | minimal except for pref.
minimisePath :: Trans -> Text -> [Int] -> Trans
minimisePath t word wordPath = minimiseZippedPath t zipped
    where
        zipped = zipPath word wordPath

-- | zips path into (from, letter, to) tuples
zipPath :: Text -> [Int] -> [(Int, Char, Int)]
zipPath word wordPath = zipWith
    (\(x, y) z -> (x, y, z))
    (zip wordPath (T.unpack word)) (tail wordPath)

-- | unzips path from (from, letter, to) tuples. Needs a starting state to
-- | handle empty paths.
unzipPath :: Int -> [(Int, Char, Int)] -> (Text, [Int])
unzipPath s path = (T.pack (map with path), s:(map to path))
    where
        with (_, a, _) = a
        to (_, _, n) = n

-- | if the transducer is minimal except for the given zipped path, it becomes
-- | minimal.
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

-- | make a path diverging from the given state
makePathAfter :: Trans -> Int -> Text -> (Trans, [Int])
makePathAfter t n "" = (t, [n])
makePathAfter t n word = (newT, n : newPath)
    where
        (newT, newPath) = makePathAfter tt m w
        (tt, m) = addState t n a

        a = T.head word
        w = T.tail word

-- | traverse from the start with the given word, returning the path travelled and the remaining suffix
traverseZipped :: Trans -> Text -> ([(Int, Char, Int)], Text)
traverseZipped t w = traverseZippedFrom t (start t) w

traverseZippedFrom :: Trans -> Int -> Text -> ([(Int, Char, Int)], Text)
traverseZippedFrom t n "" = ([], "")
traverseZippedFrom t n word = f (next t n a)
    where
        f (Just m) = ((n, a, m) : rest, suff)
            where
                (rest, suff) = traverseZippedFrom t m w
        f Nothing  = ([], word)

        a = T.head word
        w = T.tail word

-- | adds a new state after the given state with the given transition
addState :: Trans
         -> Int             -- ^ the state from which we make a transition
         -> Char            -- ^ with which symbol
         -> (Trans, Int)
addState t n a = addGivenState t n a $ State {
        transition = HashMap.empty,
        final = Nothing,
        output = HashMap.empty,
        degree = 0
    }

-- | adds a new state after the given state with the given transition
addGivenState :: Trans
         -> Int             -- ^ the state from which we make a transition
         -> Char            -- ^ with which symbol
         -> State
         -> (Trans, Int)
addGivenState t prevStateID a newState
    = (t', newStateID)
        where
            t' = addTransition prevStateID a newStateID $ t {
                states = HashMap.insert newStateID newState (states t),
                lastState = newStateID
            }

            newStateID = lastState t + 1


-- | if the target state is convergent, clone it and return the new state ID
unminimiseTransition :: Trans -> (Int, Char, Int) -> (Trans, Int)
unminimiseTransition t (m, a, n)
    | isConvergent t n =
        ( (bumpDegrees t3 (HashMap.elems $ transition $ state t3 newState))
        , newState )
    | otherwise = (delFromEquiv t n, n)
    where
        t3 = setOutput t2 m a (outEmpty t m a)
        (t2, newState) = addGivenState t1 m a (s { degree = 0 })
        s = state t1 n
        t1 = delTransition m a n t0
        t0 = delFromEquiv t m

isConvergent :: Trans -> Int -> Bool
isConvergent t n = degree (state t n) > 1

-- | checks if there's a state which is equivalent to the target state.
-- | If there is, the target state is deleted and the transition is pointed
-- | at its equivalent state
minimiseTransition :: (Int, Char, Int) -> Trans -> Trans
minimiseTransition (from, a, to) t = checkEquiv toEquiv
    where
        checkEquiv Nothing = addToEquiv t to
        checkEquiv (Just n)
            | n == to   = t   -- state is already part of the minimal path
            | otherwise = addToEquiv t' n
            where
                t' = (addTransition from a n $ delState to t)

        toEquiv = getEquiv t to

emptyExcept :: Trans -> (Trans, Except)
emptyExcept t = (t, ((T.pack ""), [start t]))
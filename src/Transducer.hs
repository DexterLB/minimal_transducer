{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Transducer where

import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

import Data.Hashable (hashWithSalt, Hashable)

import Data.Text (Text)
import qualified Data.Text as T

import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Data.List (sort)

import qualified Data.List.Unique as Uniq

import qualified JsonTransducer as JT

import Debug.Trace


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
-- state (Trans {states}) n = states HashMap.! n
state (Trans {states}) n
    | (Just s) <- HashMap.lookup n states = s
    | otherwise = error $ "state " ++ (show n) ++ " not found"


-- **** Mutations ****

addTransition :: Int -> Char -> Int -> Trans -> Trans
addTransition from a to t
    | (Just oldTo) <- HashMap.lookup a (transition (state t from))
        = updateState (
            updateDegree (
              updateDegree t oldTo (\x -> x - 1)
            ) to (+ 1)
          ) from f
    | otherwise = updateState (updateDegree t to (+ 1)) from f
    where
        f state = state {
                transition = HashMap.insert a to (transition state),
                output = bump a "" (output state)
            }


delTransition :: Int -> Char -> Int -> Trans -> Trans
delTransition from a to t
    -- | HashMap.lookup a (transition $ state t from) == Just to
         = updateState (updateDegree t to (\x -> x - 1)) from f
    -- | otherwise = undefined -- invariant
    where
        f state = state {
                transition = HashMap.delete a (transition state),
                output     = HashMap.delete a (output state)
            }

delTransitionsFor :: Trans -> Int -> Trans
delTransitionsFor t n = foldr f t outgoing
    where
        f :: (Int, Char, Int) -> Trans -> Trans
        f (m, a, n) t = delTransition m a n t

        outgoing = map (\(a, m) -> (n, a, m)) $ HashMap.toList (transition $ state t n)

bumpDegrees :: Trans -> [Int] -> Trans
bumpDegrees t = foldr bumpDegree t

bumpDegree :: Int -> Trans -> Trans
bumpDegree n t = updateDegree t n (+ 1)

bump :: (Eq k, Hashable k) => k -> v -> HashMap k v -> HashMap k v
bump key newValue m
    | HashMap.member key m  = m
    | otherwise             = HashMap.insert key newValue m

delState :: Int -> Trans -> Trans
delState n t = t' {
        states = HashMap.delete n (states t')
    }
    where
        t' = delFromEquiv (delTransitionsFor t n) n


prependToOutputs :: Trans -> Int -> Text -> Trans
prependToOutputs t _ "" = t
prependToOutputs t n out = updateState t n f
    where
        f state = state {
            output = HashMap.map (out `T.append`) (output state),
            final = (out `T.append`) <$> (final state)
        }

appendToOutput :: Trans -> Int -> Char -> Text -> Trans
appendToOutput t _ _ "" = t
appendToOutput t n a suffix = updateState t n f
    where
        f state = state {
            output = HashMap.adjust (\x -> T.append x suffix) a (output state)
        }

commonOutputPrefix :: Trans -> Int -> Text
commonOutputPrefix t n
    | outputs == [] = ""
    | otherwise = foldr1 lcp outputs
    where
        outputs
            | (Just out) <- final s = out : transitionOutputs
            | otherwise             = transitionOutputs
        transitionOutputs = HashMap.elems $ output s
        s = state t n

removeOutputPrefix :: Trans -> Int -> Text -> Trans
removeOutputPrefix t _ "" = t
removeOutputPrefix t n prefix = updateState t n f
    where
        f state = state {
            output = HashMap.map (removePrefix prefix) (output state),
            final = (removePrefix prefix) <$> (final state)
        }




setFinal :: Trans -> Int -> Text -> Trans
setFinal t n out = updateState t n f
    where
        f state = state {
            final = Just out
        }

unFinal :: Trans -> Int -> Trans
unFinal t n = updateState t n f
    where
        f state = state {
            final = Nothing
        }

setOutput :: Trans -> Int -> Char -> Text -> Trans
setOutput t n a out = updateState t n f
    where
        f state = state {
            output = HashMap.insert a out (output state)
        }

-- | updates a state both in the state table
-- updateState :: Trans -> Int -> (State -> State) -> Trans
-- updateState t n f
--     | not $ HashMap.member n (states t) = error "fck u"
-- -- equiv doesn't need updating because updateState is only called
-- -- before inserting a state in equiv. uncomment the following check to verify:
-- --    | HashMap.lookup (state t n) (equiv t) == Just n = error $ "trying to update state " ++ (show n) ++ " which is in equiv"
--     | otherwise = t {
--             states = HashMap.insert n newState (states t)
--         }
--     where
--         newState = f oldState
--         oldState = state t n

-- | updates a state both in the state table
updateState :: Trans -> Int -> (State -> State) -> Trans
updateState t n f = t {
        states = HashMap.adjust f n (states t)
    }

updateDegree :: Trans -> Int -> (Int -> Int) -> Trans
updateDegree t n f  = t {
            states = HashMap.adjust update n (states t)
        }
    where
        update s = s { degree = f (degree s) }



-- **** Prints ****

instance Show Trans where
    show = (foldr (++) "") . (map (++ "\n")) . showTransLines

instance Show State where
    show = (foldr (++) "") . (map (++ "\n")) . ((curry showStateLines) (-1))

showState :: (Int, State) -> String
showState = unlines . showStateLines

showTransLines :: Trans -> [String]
showTransLines t
    =  (showTransDataLines t)
    ++ (showVerifyEquivLines t)

dotifyTrans :: Trans -> String
dotifyTrans = unlines . dotifyTransLines

dotifyTransLines :: Trans -> [String]
dotifyTransLines (Trans {states, start})
    = ["digraph trans {"]
    ++ (map ("    " ++ ) (foldr (++) [] (map dotifyState (HashMap.toList states))))
    ++ ["}"]

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

dotifyState :: (Int, State) -> [String]
dotifyState (n, State {transition, final, output, degree})
    =  [(show n) ++ " [label=\"" ++ (show n) ++ (showFinalOutput final) ++ " [" ++ (show degree) ++ "]\"];"]
    ++ (map
        (dotifyTransition n)
        (zipTransitions (State {transition, final, output, degree}))
       )

dotifyTransition :: Int -> (Char, Int, Maybe Text) -> String
dotifyTransition n (c, i, Just o) = (show n) ++ " -> " ++ (show i) ++ " [label=\"" ++ (c:":") ++ (T.unpack o) ++ "\"];"
dotifyTransition n (c, i, Nothing) = (show n) ++ " -> " ++ (show i) ++ " [label=\"" ++ (c:"\"];")

zipTransitions :: State -> [(Char, Int, Maybe Text)]
zipTransitions (State {transition, final, output}) = map getTransition keys
    where
        keys = HashMap.keys transition
        getTransition c = (c, transition HashMap.! c, HashMap.lookup c output)

delFromEquiv :: Trans -> Int -> Trans
delFromEquiv t n = t {
            equiv = HashMap.alter f (state t n) (equiv t)
        }
    where
        f Nothing = Nothing
        f (Just x)
            | x == n = Nothing
            | otherwise = Just x

addToEquiv :: Trans -> Int -> Trans
addToEquiv t n = t {
        equiv = HashMap.insert (state t n) n (equiv t)
    }

getEquiv :: Trans -> Int -> Maybe Int
getEquiv t n = HashMap.lookup (state t n) (equiv t)

showVerifyEquivLines :: Trans -> [String]
showVerifyEquivLines t
    | verifyEquiv t = []
    | otherwise     = ["  WARN: transducer has wrong equivalence set"]

verifyEquiv :: Trans -> Bool
verifyEquiv Trans {states, equiv, start} = (
        HashMap.fromList $ map (\(x, y) -> (y, x)) $ filter (\(x, _) -> x /= start) (HashMap.toList states)
    ) == equiv

showMapLines :: (Show k, Show v) => HashMap k v -> [String]
showMapLines = (map showPair) . HashMap.toList
    where
        showPair (k, v) = (show k) ++ " -> " ++ (show v)

showFinalLines :: (Show o) => Maybe o -> [String]
showFinalLines Nothing = []
showFinalLines (Just output) = ["  final with output " ++ (show output)]

showFinalOutput :: Maybe Text -> String
showFinalOutput Nothing = ""
showFinalOutput (Just output) = " -> " ++ (T.unpack output)

transitionCount :: Trans -> Int
transitionCount Trans {states} = sum $ HashMap.map (\s -> HashMap.size $ transition s) states

stateCount :: Trans -> Int
stateCount t = HashMap.size $ states t

dumpDic :: Trans -> [(Text, Text)]
dumpDic t = dumpDic' t (start t) ("", "")
dumpDic' :: Trans -> Int -> (Text, Text) -> [(Text, Text)]
dumpDic' t n (w, o)
    | (Just t) <- final (state t n) = (w, T.append o t) : rest
    | otherwise = rest
    where
        rest = foldr (++) [] langs

        langs = map lang letters

        lang letter = dumpDic'
            t
            ((transition $ state t n) HashMap.! letter)
            ((T.snoc w letter), (T.append o ((output $ state t n) HashMap.! letter)))

        letters = sort $ HashMap.keys $ transition $ state t n



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



-- **** conversion ****

toJsonTrans :: Trans -> JT.Trans
toJsonTrans t = JT.Trans {
    alphabet = alphabet,
    states = Vector.map (toJsonTransState t stateIndex outputIndex alphabetIndex)
            $ oldStates,
    possibleOutputs = outputs
}
    where
        (oldStates, stateIndex) = indexedStates t
        (outputs, outputIndex) = indexedArray $ allOutputs t
        (alphabet, alphabetIndex) = indexedAlphabet t

toJsonTransState :: Trans
                 -> HashMap State Int
                 -> HashMap Text Int
                 -> HashMap Char Int
                 -> State -> JT.State

toJsonTransState t stateIndex outputIndex alphabetIndex state
    = JT.State {
        final = finalIndexOutput outputIndex (final state),
        transitions = HashMap.fromList
            $ map (toJsonTransTransition t stateIndex outputIndex alphabetIndex)
            $ transitionTuples
    }
    where
        transitionTuples = map addOutput $ HashMap.toList $ transition state

        addOutput :: (Char, Int) -> (Char, Int, Text)
        addOutput (c, next) = (c, next, (output state HashMap.! c))

toJsonTransTransition :: Trans
                      -> HashMap State Int
                      -> HashMap Text Int
                      -> HashMap Char Int
                      -> (Char, Int, Text)
                      -> (Int, JT.Target)
toJsonTransTransition t stateIndex outputIndex alphabetIndex (with, to, out)
    = ((alphabetIndex HashMap.! with), target)
    where
        target = JT.Target {
            state   = stateIndex HashMap.! (states t HashMap.! to),
            output  = outputIndex HashMap.! out
        }

finalIndexOutput :: HashMap Text Int -> Maybe Text -> Int
finalIndexOutput _ Nothing = -1
finalIndexOutput outputIndex (Just output) = outputIndex HashMap.! output

indexedAlphabet :: Trans -> (Text, HashMap Char Int)
indexedAlphabet t = (T.pack sorted, map)
    where
        sorted  = sortedAlphabet t
        map     = HashMap.fromList $ zip sorted [0..]

sortedAlphabet :: Trans -> [Char]
sortedAlphabet t = Uniq.sortUniq allLabels
    where
        allLabels = concat $ map (HashMap.keys . transition)
                    $ HashMap.elems (states t)

indexedArray :: (Eq a, Hashable a) => [a] -> (Vector a, HashMap a Int)
indexedArray l = (Vector.fromList elems, HashMap.fromList $ zip elems [0..])
    where
        elems = HashMap.keys $ HashMap.fromList $ zip l (repeat Void)

allOutputs :: Trans -> [Text]
allOutputs t = concat $ map allStateOutputs $ HashMap.elems $ states t

allStateOutputs :: State -> [Text]
allStateOutputs state = maybePrepend (final state)
                        $ HashMap.elems $ output state

indexedStates :: Trans -> (Vector State, HashMap State Int)
indexedStates t = (Vector.fromList states, HashMap.fromList $ zip states [0..])
    where
        states = sortedStates t

-- | returns all states of the transducer, starting with the start state
sortedStates :: Trans -> [State]
sortedStates t = check $ HashMap.lookup (start t) (states t)
    where
        check :: Maybe State -> [State]
        check Nothing = []
        check (Just startState) = startState : (HashMap.elems
            $ HashMap.delete (start t) (states t))

data Void = Void Void

-- **** Data ****

data State = State {
    transition :: HashMap Char Int,
    final :: Maybe Text,
    output :: HashMap Char Text,
    degree :: Int
}

instance Eq State where
    x == y = (transition x, final x, output x) == (transition y, final y, output y)

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

maybePrepend :: (Maybe a) -> [a] -> [a]
maybePrepend Nothing l = l
maybePrepend (Just elem) l = elem : l

emptyTrans :: Trans
emptyTrans = Trans {
    start = 1,
    states = HashMap.fromList [
        (1, State (HashMap.empty) Nothing (HashMap.empty) 0)
    ],
    equiv = HashMap.fromList [],
    lastState = 1
}

-- **** utils ****

removePrefix :: Text -> Text -> Text
removePrefix prefix t
    | (Just suffix) <- T.stripPrefix prefix t = suffix
    | otherwise = undefined

-- | longest common prefix
lcp :: Text -> Text -> Text
lcp a b = fst3 (lcprefixes a b)

lcprefixes :: Text -> Text -> (Text, Text, Text)
lcprefixes a b = couldBeEmpty (T.commonPrefixes a b)
    where
        couldBeEmpty Nothing            = (T.empty, a, b)
        couldBeEmpty (Just prefixes)    = prefixes

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Transducer
import Minimal

import Data.Char (isSpace)
import System.Environment (getArgs)

import Data.Text (Text)
import qualified Data.Text as T

-- | read a dictionary file (the first argument), and then read words from
-- | stdin and print their outputs to stdout
main :: IO ()
main = do
    args <- getArgs
    dic <- readDic $ args !! 0
    
    let t = minimalTransducer dic

    interact $ prompt t

prompt :: Trans -> String -> String
prompt t = 
    unlines . 
    (("built transducer with " ++ (show $ length $ states t) ++ " states.") :) .
    (map (linePrompt t)) . lines

linePrompt :: Trans -> String -> String
linePrompt t = (" -> " ++) . T.unpack . deMaybe . (match t) . T.pack

readDic :: String -> IO [(Text, Text)]
readDic filename = parseDic <$> (readFile filename)

parseDic :: String -> [(Text, Text)]
parseDic = splitLines . (map T.pack) . lines

splitLines :: [Text] -> [(Text, Text)]
splitLines = map splitLine

splitLine :: Text -> (Text, Text)
splitLine s = (
                   T.takeWhile (not . isSpace) s, 
        T.drop 1 $ T.dropWhile (not . isSpace) s
    )

deMaybe :: Maybe Text -> Text
deMaybe Nothing = T.pack "¯\\_(ツ)_/¯"
deMaybe (Just s) = s

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Transducer
import Minimal

import Data.Char (isSpace)
import System.Environment (getArgs)

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
linePrompt t = (" -> " ++) . deMaybe . (match t)

readDic :: String -> IO [(String, String)]
readDic filename = parseDic <$> (readFile filename)

parseDic :: String -> [(String, String)]
parseDic = splitLines . lines

splitLines :: [String] -> [(String, String)]
splitLines = map splitLine

splitLine :: String -> (String, String)
splitLine s = (
                 takeWhile (not . isSpace) s, 
        drop 1 $ dropWhile (not . isSpace) s
    )

deMaybe :: Maybe String -> String
deMaybe Nothing = "¯\\_(ツ)_/¯"
deMaybe (Just s) = s

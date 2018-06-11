{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Transducer
import Minimal
import qualified JsonTransducer as JT

import Data.Char (isSpace)
import System.Environment (getArgs)


import Data.Conduit (runConduitRes, (.|) )
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad (foldM)
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as Co

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TI

import Data.Time.Clock

import System.IO (hPutStr, stderr)


import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Lazy.Char8 as ByteString8

import qualified Data.Aeson as Aeson

import qualified Data.HashMap.Strict as HashMap

-- | read a dictionary file (the first argument), and then read words from
-- | stdin and print their outputs to stdout
main :: IO ()
main = do
    args <- getArgs

    foldM processArg emptyTrans args

    return ()


processArg :: Trans -> String -> IO Trans
processArg t ('j':'s':'o':'n':':':filename) = do
    ByteString8.writeFile filename $ jsonify t
    return t
processArg t ('s':'o':'r':'t':'e':'d':':':dic) = do
    result <- processDic (emptyExcept t) addWordI (simpleProgress . fst) dic
    return $ finalise result
processArg t ('a':'d':'d':':':dic) = processDic t addWordU simpleProgress dic
processArg t ('d':'e':'l':':':dic) = processDic t del simpleProgress dic
    where
        del t (word, _) = delWordU t word
processArg t "prompt" = do
    doPrompt t
    return t
processArg t "info" = do
    putStr $ info t
    return t
processArg t "dump_outputs" = do
    putStrLn $ unlines $ map (show . HashMap.elems . output) $ HashMap.elems $ states t
    return t
processArg t "dump_dic" = do
    putStrLn $ T.unpack $ dump t
    return t
processArg t ('d':'o':'t':':':filename) = do
    writeFile filename $ dotifyTrans t
    return t


simpleProgress :: Trans -> String
simpleProgress t = "states: " ++ (show $ stateCount t)

countLines :: String -> IO Int
countLines file = do
    allText <- TI.readFile file
    return $ length $ TL.lines allText


processDic :: a -> (a -> (Text, Text) -> a) -> (a -> String) -> String -> IO a
processDic t f prog file = do
    startTime <- getCurrentTime
    allText <- TI.readFile file
    let lines = map TL.toStrict $ TL.lines allText
    numLines <- countLines file
    (_, [], !t) <- processChunk f prog (numLines) (0, lines, t)
    endTime <- getCurrentTime

    hPutStr stderr $ "that took " ++ (show $ diffUTCTime endTime startTime) ++ "\n"

    return t

processChunk :: (a -> (Text, Text) -> a) -> (a -> String) -> Int -> (Int, [Text], a) -> IO (Int, [Text], a)
processChunk f prog _ (n, [], t) = do
    hPutStr stderr "\n"
    return (n, [], t)

processChunk f prog total (n, line:lines, !t) = do
    let t' = f t (splitLine line)
    case (n `mod` 100000) == 0 || n == total - 1 of
        True -> hPutStr stderr $ "\rprogress: " ++ (show (n + 1)) ++ "/" ++ (show total) ++ " (" ++ (show $ ((n + 1) * 100) `div` total) ++ "%) " ++ (prog t') ++ "        "
        False -> pure ()

    result <- processChunk f prog total (n + 1, lines, t')
    return result

doPrompt :: Trans -> IO ()
doPrompt = interact . prompt

jsonify :: Trans -> ByteString
jsonify t = Aeson.encode $ toJsonTrans t

dump :: Trans -> Text
dump t = T.intercalate "\n" lines
    where
        lines :: [Text]
        lines = map (\(w, o) -> T.append (T.append w "\t") o) $ dumpDic t

info :: Trans -> String
info t =  "built transducer with "
        ++ (show $ stateCount t)
        ++ " states and "
        ++ (show $ transitionCount t)
        ++ " transitions.\n"
        ++ (unlines $ showVerifyEquivLines t)

prompt :: Trans -> String -> String
prompt t =
    unlines .
    ((
        info t ++
        "converted transducer to JsonTransducer with "
        ++ (show $ length $ JT.states jt)
        ++ " states."
    ) :) .
    (map (linePrompt jt)) . lines
    where
        jt = toJsonTrans t

linePrompt :: JT.Trans -> String -> String
linePrompt jt = (" -> " ++) . T.unpack . deMaybe . (JT.match jt) . T.pack

splitLine :: Text -> (Text, Text)
splitLine s = (
                   T.takeWhile (not . ( == '\t')) s,
        T.drop 1 $ T.dropWhile (not . ( == '\t')) s
    )

deMaybe :: Maybe Text -> Text
deMaybe Nothing = T.pack "¯\\_(ツ)_/¯"
deMaybe (Just s) = s

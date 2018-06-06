{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Lazy.Char8 as ByteString8

import qualified Data.Aeson as Aeson

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
    result <- processDic (emptyExcept t) addWordI dic
    return $ finalise result
processArg t ('a':'d':'d':':':dic) = processDic t addWordU dic
processArg t ('d':'e':'l':':':dic) = processDic t del dic
    where
        del t (word, _) = delWordU t word
processArg t "prompt" = do
    doPrompt t
    return t
processArg t "info" = do
    putStr $ info t
    return t
processArg t ('d':'o':'t':':':filename) = do
    writeFile filename $ dotifyTrans t
    return t



processDic :: a -> (a -> (Text, Text) -> a) -> String -> IO a
processDic t f file = runConduitRes $
        (Co.sourceFile  file )
    .| Co.decodeUtf8
    .| Co.linesUnbounded
    .| Co.map splitLine
    .| Co.foldl f t

doPrompt :: Trans -> IO ()
doPrompt = interact . prompt

jsonify :: Trans -> ByteString
jsonify t = Aeson.encode $ toJsonTrans t

info :: Trans -> String
info t =  "build transducer with "
        ++ (show $ length $ states t)
        ++ " states and "
        ++ (show $ transitionCount t)
        ++ " transitions.\n"

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
                   T.takeWhile (not . isSpace) s,
        T.drop 1 $ T.dropWhile (not . isSpace) s
    )

deMaybe :: Maybe Text -> Text
deMaybe Nothing = T.pack "¯\\_(ツ)_/¯"
deMaybe (Just s) = s

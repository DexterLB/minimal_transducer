{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Transducer
import Minimal
import qualified JsonTransducer as JT

import Data.Char (isSpace)
import System.Environment (getArgs)


import Data.Conduit (runConduit, (.|) , ($$) )
import Control.Monad.Trans.Resource (runResourceT)
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

    t <- runResourceT $
              (Co.sourceFile  (args !! 0) )
            $$ Co.decodeUtf8
            .| Co.linesUnbounded
            .| Co.map splitLine
            .| Co.foldl addWordI emptyExcept

    let minimalT = finalise t

    useTrans args minimalT

useTrans :: [String] -> Trans -> IO ()
useTrans args t
    | (length args < 2) || ((args !! 1) /= "-j") 
        = interact $ prompt t
    | otherwise = ByteString8.putStrLn $ jsonify t

jsonify :: Trans -> ByteString
jsonify t = Aeson.encode $ toJsonTrans t

prompt :: Trans -> String -> String
prompt t = 
    unlines . 
    ((
        "built transducer with "
        ++ (show $ length $ states t) 
        ++ " states and "
        ++ (show $ transitionCount t)
        ++ " transitions."
    ) :) .
    ((
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

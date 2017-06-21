{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Transducer
import Minimal

import Data.Char (isSpace)
import System.Environment (getArgs)


import Data.Conduit (runConduit, (.|) , ($$) )
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as Co

import Data.Text (Text)
import qualified Data.Text as T

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
            .| Co.foldl addWordI (emptyTrans, ((T.pack ""), []))

    let minimalT = finalise t

    interact $ prompt minimalT

prompt :: Trans -> String -> String
prompt t = 
    unlines . 
    (("built transducer with " ++ (show $ length $ states t) ++ " states.") :) .
    (map (linePrompt t)) . lines

linePrompt :: Trans -> String -> String
linePrompt t = (" -> " ++) . T.unpack . deMaybe . (match t) . T.pack

splitLine :: Text -> (Text, Text)
splitLine s = (
                   T.takeWhile (not . isSpace) s, 
        T.drop 1 $ T.dropWhile (not . isSpace) s
    )

deMaybe :: Maybe Text -> Text
deMaybe Nothing = T.pack "¯\\_(ツ)_/¯"
deMaybe (Just s) = s

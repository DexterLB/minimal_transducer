{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Minimal where

import Transducer

-- **** utils ****

-- ^ longest common prefix
lcp :: String -> String -> String
lcp (x:xs) (y:ys)
    | x == y        = x : (lcp xs ys)
    | otherwise     = ""
lcp _ _             = ""

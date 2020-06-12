{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where

import Protolude
import Test.DocTest
import Stats

main :: IO ()
main =
  doctest
  [ "src/Stats.hs",
    "src/Data/Random.hs"
  ]

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where

import Protolude
import Test.DocTest
import Data.Mealy

main :: IO ()
main =
  doctest
  [ "src/Data/Mealy.hs",
    "src/Data/Random.hs"
  ]

module Main where

import OPMLTestCases
import AtomTestCases

import Test.Framework(defaultMain)

main :: IO ()
main = do
  tc1 <- OPMLTestCases.tests
  tc2 <- AtomTestCases.tests
  defaultMain [tc1, tc2]

module Main where

import OPMLTestCases

import Test.Framework(defaultMain)

main :: IO ()
main = do
  tc1 <- OPMLTestCases.tests
  tc2 <- OPMLTestCases.tests
  defaultMain [tc1, tc2]

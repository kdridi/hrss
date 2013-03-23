module Main where

import Web.HRSS

import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests =
  [ testGroup "checkConfiguration"
    [ testProperty "VERSION: 0.0.0.1"   $ prop_hrssVersion $ "0.0.0.1"
    , testProperty "VERSION: 0.0.0.1"   $ prop_hrssVersion $ "0.0.0.1"
    ]
  , testGroup "checkConfiguration"
    [ testProperty "VERSION: 0.0.0.1"   $ prop_hrssVersion $ "0.0.0.1"
    ]
  ]
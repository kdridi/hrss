module Web.HRSS where

hrssVersion :: String
hrssVersion = "0.0.0.1"

prop_hrssVersion :: String -> Bool
prop_hrssVersion = (==) hrssVersion

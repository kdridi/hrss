module Main where

import Control.Monad(forM)

import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs(NTree)

import Web.HRSS.Data.OPML

parse :: IOSLA (XIOState ()) (NTree XNode) a -> String -> IO [a]
parse get xml = runX ( parseXML xml >>> get )
  where
    parseXML :: String -> IOStateArrow s b XmlTree
    parseXML doc = readString
      [ withValidate no
      , withTrace 0
      , withRemoveWS yes
      ] doc

prop_getOPML :: [OPML] -> Bool -> Int -> String -> Bool
prop_getOPML [] True _ _ = True
prop_getOPML [(OPML ot tls)] False l t = and [ ot == t, l == length tls]
prop_getOPML _ _ _ _ = False

main :: IO ()
main = do
  content <- readFile "data/test01.opml"
  opml <- parse getOPML content
  _ <- forM (map (\x -> (outlineTitle x) ++ " >> " ++ (outlineHtmlUrl x)) (opmlOutlines . head $ opml)) print
  return ()
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module Main where

import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs(NTree)

import Web.HRSS.Data.OPML

import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

xmlOPML0 :: [Char]
xmlOPML0 = "\
  \<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
  \<opml version=\"1.0\">\
  \  <head>\
  \    <title>My Title</title>\
  \  </head>\
  \</opml>"

xmlOPML1 :: [Char]
xmlOPML1 = "\
  \<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
  \<opml version=\"1.0\">\
  \  <head>\
  \    <title>My Title 1</title>\
  \  </head>\
  \  <body>\
  \    <outline text='text0' title='title0' xmlUrl='xmlUrl0' htmlUrl='htmlUrl0'/>\
  \    <outline text='text1' title='title1' xmlUrl='xmlUrl1' htmlUrl='htmlUrl1'/>\
  \  </body>\
  \</opml>"

xmlOPML2 :: [Char]
xmlOPML2 = "\
  \<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
  \<opml version=\"1.0\">\
  \  <head>\
  \    <title>My Title 2</title>\
  \  </head>\
  \  <body>\
  \    <outline text='text0' title='title0' xmlUrl='xmlUrl0' htmlUrl='htmlUrl0'/>\
  \    <outline text='text1' title='title1' xmlUrl='xmlUrl1' htmlUrl='htmlUrl1'/>\
  \    <outline text='text2' title='title2' xmlUrl='xmlUrl2' htmlUrl='htmlUrl2'/>\
  \    <outline text='text3' title='title3' xmlUrl='xmlUrl3' htmlUrl='htmlUrl3'/>\
  \  </body>\
  \</opml>"

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
  xml0 <- parse getOPML xmlOPML0
  xml1 <- parse getOPML xmlOPML1
  xml2 <- parse getOPML xmlOPML2
  defaultMain (tests [xml0, xml1, xml2])

tests :: [[OPML]] -> [TF.Test]
tests xmlOPMLs =
  [ testGroup "prop_getOPML_Error"
    [ testProperty "Error: xmlOPML0" $ prop_getOPML (xmlOPMLs !! 0) True 0
    ]
  , testGroup "prop_getOPML_NoError"
    [ testProperty "NoError: xmlOPML1" $ prop_getOPML (xmlOPMLs !! 1) False 2 "My Title 1"
    , testProperty "NoError: xmlOPML2" $ prop_getOPML (xmlOPMLs !! 2) False 4 "My Title 2"
    ]
  ]
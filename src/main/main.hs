module Main where

import Control.Monad(forM)
import Network.HTTP(getResponseBody,simpleHTTP,getRequest)

import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs(NTree)

import Web.HRSS.Data.OPML
import Web.HRSS.Data.Atom

parse :: IOSLA (XIOState ()) (NTree XNode) a -> String -> IO [a]
parse get xml = runX ( parseXML xml >>> get )
  where
    parseXML :: String -> IOStateArrow s b XmlTree
    parseXML doc = readString
      [ withValidate no
      , withTrace 0
      , withRemoveWS yes
      ] doc

readHTTP :: String -> IO String
readHTTP url = getResponseBody =<< simpleHTTP (getRequest url)

main :: IO ()
main = do
  parseOPML "data/test01.opml"
  return ()

  where
    parseOPML :: FilePath -> IO ()
    parseOPML path = do
      content <- readFile path
      opml <- parse getOPML content
      _ <- forM (opmlOutlines . head $ opml) parseAtom
      return ()

    parseAtom :: Outline -> IO ()
    parseAtom outline = do
      putStrLn $ outlineTitle outline
      content <- readHTTP $ outlineXmlUrl outline
      atom <- parse getAtom content
      print atom
      return ()

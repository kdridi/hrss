{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module Web.HRSS.Data.Atom where

import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs(NTree)

data Atom = Atom
  { atomTitle :: String
  , atomEntries :: [Entry]
  , atomLinks :: [Link]
  }
  deriving (Show, Read, Eq)

data Link = Link
  { linkRel :: String
  , linkType :: String
  , linkHref :: String
  }
  deriving (Show, Read, Eq)

data Entry = Entry
  { entryTitle :: String
  , entryLinks :: [Link]
  , entryUpdated :: String
  , entryPublished :: String
  , entryId :: String
  }
  deriving (Show, Read, Eq)

getAtom :: ArrowXml cat => cat XmlTree Atom
getAtom = atElem "feed" >>> parseAtom
  where
    parseAtom :: ArrowXml cat => cat (NTree XNode) Atom
    parseAtom = proc x -> do
      title   <- atElemText "title" -< x
      entries <- getEntries         -< x
      links   <- getLinks           -< x
      returnA -< Atom title entries links

    getLinks :: ArrowXml cat => cat XmlTree [Link]
    getLinks = listA ( atElem "link" >>> parseLink )

    parseLink :: ArrowXml cat => cat XmlTree Link
    parseLink = proc x -> do
      r <- atAttr "rel" -< x
      t <- atAttr "type" -< x
      h <- atAttr "href" -< x
      returnA -< Link r t h

    getEntries :: ArrowXml cat => cat XmlTree [Entry]
    getEntries = listA ( atElem "entry" >>> parseEntry )

    parseEntry :: ArrowXml cat => cat XmlTree Entry
    parseEntry = proc x -> do
      i <- atElemText "id"     -< x
      t <- atElemText "title"     -< x
      l <- getLinks               -< x
      u <- atElemText "updated"   -< x
      p <- atElemText "published" -< x
      returnA -< Entry t l u p i

    atAttr :: ArrowXml cat => String -> cat XmlTree String
    atAttr name = hasAttr name >>> getAttrValue name

    atElem :: ArrowXml cat => String -> cat (NTree XNode) XmlTree
    atElem name = getChildren >>> isElem >>> hasName name

    atElemText :: ArrowXml cat => String -> cat (NTree XNode) String
    atElemText name = atElem name >>> getChildren >>> getText


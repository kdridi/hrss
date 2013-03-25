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
    parseAtom :: ArrowXml cat => cat XmlTree Atom
    parseAtom = proc x -> do
      t <- (      (atElem "title" >>> parse      )) -< x
      e <- (listA (atElem "entry" >>> parseEntry )) -< x
      l <- (listA (atElem  "link" >>> parseLink  )) -< x
      returnA -< Atom t e l

    parseLink :: ArrowXml cat => cat XmlTree Link
    parseLink = proc x -> do
      r <- atAttr "rel" -< x
      t <- atAttr "type" -< x
      h <- atAttr "href" -< x
      returnA -< Link r t h

    parseEntry :: ArrowXml cat => cat XmlTree Entry
    parseEntry = proc x -> do
      i <- (      (atElem        "id" >>> parse      )) -< x
      t <- (      (atElem     "title" >>> parse      )) -< x
      l <- (listA (atElem      "link" >>> parseLink  )) -< x
      u <- (      (atElem   "updated" >>> parse      )) -< x
      p <- (      (atElem "published" >>> parse      )) -< x
      returnA -< Entry t l u p i

    parse :: ArrowXml cat => cat XmlTree String
    parse = getChildren >>> getText

    atAttr :: ArrowXml cat => String -> cat XmlTree String
    atAttr name = hasAttr name >>> getAttrValue name

    atElem :: ArrowXml cat => String -> cat (NTree XNode) XmlTree
    atElem name = getChildren >>> isElem >>> hasName name


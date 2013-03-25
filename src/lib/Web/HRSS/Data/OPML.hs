{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module Web.HRSS.Data.OPML where

import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs(NTree)

data OPML = OPML
  { opmlTitle      :: String
  , opmlOutlines   :: Outlines
  }
  deriving (Show, Eq)

type Outlines = [Outline]

data Outline = Outline
  { outlineText    :: String
  , outlineTitle   :: String
  , outlineXmlUrl  :: String
  , outlineHtmlUrl :: String
  }
  deriving (Show, Eq)

getOPML :: ArrowXml cat => cat XmlTree OPML
getOPML = getChildren >>> isElem >>> hasName "opml" >>> parseOpml
  where
    parseOpml :: ArrowXml cat => cat (NTree XNode) OPML
    parseOpml = proc x -> do
      outlines  <- getOutlines -< x
      title     <- getTitle    -< x
      returnA   -< OPML title outlines

    getTitle :: ArrowXml cat => cat XmlTree String
    getTitle = atElem "head" >>> atElem "title" >>> getChildren >>> getText

    getOutlines :: ArrowXml cat => cat XmlTree [Outline]
    getOutlines = atElem "body" >>> listA (getChildren >>> getOutline)

    getOutline :: ArrowXml cat => cat XmlTree Outline
    getOutline = isElem >>> hasName "outline" >>> parseOutline

    parseOutline :: ArrowXml cat => cat XmlTree Outline
    parseOutline = proc x -> do
      text      <- atAttr    "text" -< x
      title     <- atAttr   "title" -< x
      xmlUrl    <- atAttr  "xmlUrl" -< x
      htmlUrl   <- atAttr "htmlUrl" -< x
      returnA   -< Outline text title xmlUrl htmlUrl

    atElem :: ArrowXml cat => String -> cat (NTree XNode) XmlTree
    atElem name = getChildren >>> isElem >>> hasName name

    atAttr :: ArrowXml cat => String -> cat XmlTree String
    atAttr name = hasAttr name >>> getAttrValue name

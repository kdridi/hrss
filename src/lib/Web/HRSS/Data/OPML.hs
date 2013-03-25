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
getOPML = atElem "opml" >>> parseOpml
  where
    parseOpml :: ArrowXml cat => cat XmlTree OPML
    parseOpml = proc x -> do
      t <- (atElem "head" >>> (      (atElem   "title" >>> parse        ))) -< x
      o <- (atElem "body" >>> (listA (atElem "outline" >>> parseOutline ))) -< x
      returnA   -< OPML t o

    parseOutline :: ArrowXml cat => cat XmlTree Outline
    parseOutline = proc x -> do
      te <- atAttr    "text" -< x
      ti <- atAttr   "title" -< x
      xu <- atAttr  "xmlUrl" -< x
      hu <- atAttr "htmlUrl" -< x
      returnA   -< Outline te ti xu hu

    parse :: ArrowXml cat => cat XmlTree String
    parse = getChildren >>> getText

    atAttr :: ArrowXml cat => String -> cat XmlTree String
    atAttr name = hasAttr name >>> getAttrValue name

    atElem :: ArrowXml cat => String -> cat (NTree XNode) XmlTree
    atElem name = getChildren >>> isElem >>> hasName name

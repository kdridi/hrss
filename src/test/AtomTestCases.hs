{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module AtomTestCases where

import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs(NTree)

import Web.HRSS.Data.Atom

import Test.Framework as TF (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

xmlAtom0 :: [Char]
xmlAtom0 = "\
  \<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
  \<feed xmlns='http://www.w3.org/2005/Atom' xmlns:thr='http://purl.org/syndication/thread/1.0' xml:lang='en' xml:base='http://www.1point2vue.com/wp-atom.php'>\
  \  <title type='text'>1point2vue</title>\
  \</feed>"

xmlAtom1 :: [Char]
xmlAtom1 = "\
  \<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
  \<feed xmlns='http://www.w3.org/2005/Atom' xmlns:thr='http://purl.org/syndication/thread/1.0' xml:lang='en' xml:base='http://www.1point2vue.com/wp-atom.php'>\
  \  <title type='text'>mon titre</title>\
  \  <subtitle type='text'>Apprendre Ã  faire  des photos et Ã  les retoucher</subtitle>\
  \  <updated>2013-03-20T18:53:12Z</updated>\
  \  <link rel='alternate' type='text/html' href='http://www.1point2vue.com'/>\
  \  <id>http://www.1point2vue.com/feed/atom/</id>\
  \  <link rel='self' type='application/atom+xml' href='http://www.1point2vue.com/feed/atom/'/>\
  \  <generator uri='http://wordpress.org/' version='3.3.1'>WordPress</generator>\
  \  <entry>\
  \    <author>\
  \      <name>Antoine</name>\
  \      <uri>http://www.1point2vue.com</uri>\
  \    </author>\
  \    <title type='html'>Le projet photo: un outil pour faÃ§onner l&#8217;experience du photographe</title>\
  \    <link rel='alternate' type='text/html' href='http://www.1point2vue.com/projet-photo-experience/'/>\
  \    <id>http://www.1point2vue.com/?p=11026</id>\
  \    <updated>2013-03-20T18:53:12Z</updated>\
  \    <published>2013-03-20T18:51:02Z</published>\
  \    <category scheme='http://www.1point2vue.com' term='projet photo'/>\
  \    <category scheme='http://www.1point2vue.com' term='wordpress'/>\
  \    <summary type='html'>S'imposer un projet photo est une faÃ§on de consolider son experience de la photo. DÃ©couvrez par quels moyens vous pouvez devenir un meilleur photographe simplement en ajoutant quelques contraintes Ã  votre pratique de la photo.<br/><br/>Lire l'article <a href='http://www.1point2vue.com/projet-photo-experience/'>Le projet photo: un outil pour faÃ§onner l&#8217;experience du photographe</a><br /><hr /><em>Le bon plan du moment: <a href='http://ad.zanox.com/ppc/?17906432C82208704&#038;zpar9=168E7CE40089AE6CAF3B'>80 photos offertes pour les nouveaux inscrit sur MyPix.com</a></em><hr /></summary>\
  \    <link rel='replies' type='text/html' href='http://www.1point2vue.com/projet-photo-experience/#comments' thr:count='9'/>\
  \    <link rel='replies' type='application/atom+xml' href='http://www.1point2vue.com/projet-photo-experience/feed/atom/' thr:count='9'/>\
  \    <thr:total>9</thr:total>\
  \  </entry>\
  \  <entry>\
  \    <author>\
  \      <name>Antoine</name>\
  \      <uri>http://www.1point2vue.com</uri>\
  \    </author>\
  \    <title type='html'>RÃ©aliser un panographe avec Gimp</title>\
  \    <link rel='alternate' type='text/html' href='http://www.1point2vue.com/panographe-avec-gimp/'/>\
  \    <id>http://www.1point2vue.com/?p=10953</id>\
  \    <updated>2013-01-26T18:01:13Z</updated>\
  \    <published>2013-01-26T18:01:13Z</published>\
  \    <category scheme='http://www.1point2vue.com' term='Graphisme'/>\
  \    <category scheme='http://www.1point2vue.com' term='assemblage'/>\
  \    <category scheme='http://www.1point2vue.com' term='deplacement'/>\
  \    <category scheme='http://www.1point2vue.com' term='la boite Ã  photo'/>\
  \    <category scheme='http://www.1point2vue.com' term='Panographe'/>\
  \    <category scheme='http://www.1point2vue.com' term='rotation'/>\
  \    <summary type='html'>Le panographe est une autre faÃ§on de faire de la photo panoramique. Bien plus simple du point de vue de la prise de vue, il permet d'obtenir des effets vraiment originaux.<br/><br/>Lire l'article <a href='http://www.1point2vue.com/panographe-avec-gimp/'>RÃ©aliser un panographe avec Gimp</a><br /><hr /><em>Le bon plan du moment: <a href='http://ad.zanox.com/ppc/?17906432C82208704&#038;zpar9=168E7CE40089AE6CAF3B'>80 photos offertes pour les nouveaux inscrit sur MyPix.com</a></em><hr /></summary>\
  \    <link rel='replies' type='text/html' href='http://www.1point2vue.com/panographe-avec-gimp/#comments' thr:count='7'/>\
  \    <link rel='replies' type='application/atom+xml' href='http://www.1point2vue.com/panographe-avec-gimp/feed/atom/' thr:count='7'/>\
  \    <thr:total>7</thr:total>\
  \  </entry>\
  \</feed>"

parse :: IOSLA (XIOState ()) (NTree XNode) a -> String -> IO [a]
parse get xml = runX ( parseXML xml >>> get )
  where
    parseXML :: String -> IOStateArrow s b XmlTree
    parseXML doc = readString
      [ withValidate no
      , withTrace 0
      , withRemoveWS yes
      ] doc

prop_getAtom :: [Atom] -> Bool -> String -> Int -> Int -> Bool
prop_getAtom [] True _ _ _ = True
prop_getAtom [(Atom t es ls)] False tt tel tll = and [ tt == t, tel == length es, tll == length ls]
prop_getAtom _ _ _ _ _ = False

tests :: IO (TF.Test)
tests = do
  xml0 <- parse getAtom xmlAtom0
  xml1 <- parse getAtom xmlAtom1
  return $ testGroup "AtomTestCases"
    [ testProperty "Error: xmlAtom0"    $ prop_getAtom xml0 False "1point2vue" 0 0
    , testProperty "NoError: xmlAtom1"  $ prop_getAtom xml1 False "mon titre" 2 2
    ]

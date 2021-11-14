{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE ScopedTypeVariables,FlexibleInstances,UnicodeSyntax #-}

module Main where

import Text.XML.Light
import Data.List
import Control.Monad
import Data.Maybe

type XML = Element

readXML :: FilePath -> IO Element
readXML filepath = do
	f <- readFile filepath
	case parseXMLDoc f of
		Nothing -> error $ "ERROR parsing " ++ filepath
		Just element -> return element

writeXML :: FilePath -> XML -> IO ()
writeXML filepath xml = do
	writeFile filepath $ ppTopElement xml

main :: IO ()
main = do
	xml1 <- readXML "newtool1.tca"
	xml2 <- readXML "newtool2.tca"
	mapM putStrLn $ mergeXML xml1 xml2
	
--	writeXML "testout.xml" 

-- Determines if something should be considered as the same element when merging
class Same a where
	same :: a -> a -> Bool

class (Same a) => Mergable a where
	merge :: a -> a -> a

instance Same Element where
	same (Element name1 attribs1 contents1 line1) (Element name2 attribs2 contents2 _) =
		name1==name2 && idAttrib attribs1 == idAttrib attribs2

instance Mergable Element where
	merge elem1@(Element name1 attribs1 contents1 line1) elem2@(Element name2 attribs2 contents2 _) | elem1 `same` elem2 =
		Element name1 (mergeList attribs1 attribs2) (mergeList contents1 contents2) line1
		case name1==name2 of
			True -> case idAttrib attribs1 == idAttrib elem2 attribs2 of
				True  -> 
				False -> 

instance Same Attrib where
	same (Attr qname1 _) (Attr qname2) = qname1==qname2

instance 
instance (Mergable a) => Mergable [a] where
	merge a

idAttrib attribs = case filter ((=="id").attrKey) attribs of
	[Attr key val] -> Just val
	_              -> Nothing

mergeList :: (Eq a,Same a) => [a] -> [a] -> [a]
mergeList (a:as) (b:bs) | a `same` b = a : mergeList as bs
mergeList (
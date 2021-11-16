{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE ScopedTypeVariables,FlexibleInstances,UnicodeSyntax #-}

module Main where

import Text.XML.Light
import Data.List
--import Control.Monad
--import Data.Maybe

type XML = Element

readXML :: FilePath -> IO Element
readXML filepath = do
	f <- readFile filepath
	case parseXMLDoc f of
		Nothing -> error $ "ERROR parsing " ++ filepath
		Just element -> return element

writeXML :: FilePath -> Element -> IO ()
writeXML filepath element = do
	writeFile filepath $ ppTopElement element

main :: IO ()
main = do
	xml1 <- readXML "newtool1.tca"
	xml2 <- readXML "newtool2.tca"
	writeXML "merged.tca" $ merge [xml1] [xml2]


---- Same ----------------------	
-- Determines if something should be considered as the same element when merging

class Same a where
	same :: a -> a -> Bool

instance Same Element where
	same (Element name1 attribs1 contents1 line1) (Element name2 attribs2 contents2 _) =
		name1==name2 && idAttrib attribs1 == idAttrib attribs2
		where
		idAttrib attribs = case filter ((=="id").attrKey) attribs of
			[Attr key val] -> Just val
			_              -> Nothing

instance Same Attrib where
	same (Attr qname1 _) (Attr qname2 _) = qname1==qname2

---- Mergable -------------------

diffList :: Mergable a => [a] -> [a] -> [a]
mergeList l1 [] = l1
mergeList [] l2 = l2
mergeList (e1:l1s) (e2:l2s) | e1 `same` e2 = merge e1 e2 : mergeList l1s l2s
mergeList l1@(e1:l1s) l2@(e2:l2s) = case (first_same e1 l2s,first_same e2 l1s) of
	((inserted,rest),(_,[])) | not (null inserted) -> inserted ++ mergeList l1 rest
	((_,[]),(deleted,rest))  | not (null deleted)  -> mergeList 
	where
	first_same e l = span (not.(same e)) l

class Same a => Mergable a where
	merge :: a -> a -> [a]

instance Mergable Element where
	merge elem1@(Element name1 attribs1 contents1 line1) elem2@(Element name2 attribs2 contents2 _) | elem1 `same` elem2 =
		Element name1 (mergeList attribs1 attribs2) (mergeList contents1 contents2) line1
	merge elem1@(Element name1 attribs1 contents1 line1) elem2@(Element name2 attribs2 contents2 _)
		case name1==name2 of
			True -> case idAttrib attribs1 == idAttrib elem2 attribs2 of
				True  -> 
				False -> 

instance (Mergable a) => Mergable [a] where
	merge (a:as) (b:bs) = 



{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE ScopedTypeVariables,FlexibleInstances,UnicodeSyntax #-}

module Main where

import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.Pretty
import Text.PrettyPrint
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Parse
import Data.List

import Extsubset


type TCAFile = Xmi'XMI

readTcaFile :: FilePath -> IO TCAFile
readTcaFile filepath = fReadXml filepath

readElementTree :: FilePath -> IO (Element Posn,Element i -> Document i)
readElementTree filepath = do
	content <- readFile filepath
	let Document prolog symtab element misc = xmlParse filepath content
	return (element,\ e -> Document prolog symtab e misc)

main :: IO ()
main = do
	(base,embedbase) <- readElementTree "Branch.tca"
	(mod1,embedmod1) <- readElementTree "newtool1.tca"
	let [mergedelement] = mergeLists sameElement mergeElements [base] [mod1]
	writeFile ("merged.tca") $ render $ Text.XML.HaXml.Pretty.document $ embedmod1 mergedelement

sameAttribute (qname1,_) (qname2,_) = qname1==qname2
mergeAttributes (qname1,AttValue esrs1) (qname2,AttValue esrs2) | qname1==qname2 = (qname1,AttValue $ esrs1++esrs2)
sameElement (Elem qname1 _ _) (Elem qname2 _ _) = qname1==qname2
mergeElements (Elem qname1 attrs1 contents1) (Elem qname2 attrs2 contents2) | qname1==qname2 =
	Elem qname1 (mergeLists sameAttribute mergeAttributes attrs1 attrs2) (mergeLists sameContent mergeContents contents1 contents2)

sameContent (CElem elem1 _) (CElem elem2 _) = sameElement elem1 elem2
sameContent cont1 cont2 = cont1==cont2
mergeContents (CElem elem1 i1) (CElem elem2 i2) = CElem (mergeElements elem1 elem2) i1
mergeContents cont1 _ = cont1

mergeLists :: (a -> a -> Bool) -> (a -> a -> a) -> [a] -> [a] -> [a]
mergeLists same_elem merge_elem (elem1:rest1) (elem2:rest2) | same_elem elem1 elem2 =
	merge_elem elem1 elem2 : mergeLists same_elem merge_elem rest1 rest2
mergeLists same_elem merge_elem l1 [] = l1
mergeLists same_elem merge_elem [] l2 = l2
mergeLists same_elem merge_elem l1@(elem1:rest1) l2@(elem2:rest2) = case break (same_elem elem1) l2 of
	(_,[]) -> case break (same_elem elem2) l1 of
		(_,[]) -> l1++l2
		(_,rest) -> mergeLists same_elem merge_elem rest l2
	(insertion,rest) -> insertion ++ mergeLists same_elem merge_elem l1 rest

	
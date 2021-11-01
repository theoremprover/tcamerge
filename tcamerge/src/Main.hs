{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE ScopedTypeVariables,FlexibleInstances,UnicodeSyntax #-}

module Main where

import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.Pretty
import Text.PrettyPrint

import Extsubset


type TCAFile = Xmi'XMI

readTcaFile :: FilePath -> IO TCAFile
readTcaFile name = fReadXml xmlFileName

readElementTree :: FilePath -> IO (Element Posn,Element i -> Document i)
readElementTree file = do
	content <- readFile filepath
	let Document prolog symtab element misc = xmlParse filename content
	return (element,\ e -> Document prolog symtab e misc)

main :: IO ()
main = do
    (base,embedbase) <- readElementTree "Branch.tca"
	(mod1,embedmod1) <- readElementTree "newtool1.tca"
    let [mergedelement] = mergeList sameElement mergeElements [base] [mod1]
	writeFile ("merged_"++filepath) $ document $ render $ embedmod1 mergedelement

sameElement (Elem qname1 _ _) (Elem qname2 _ _) = qname1==qname2

mergeElements (Elem qname1 attrs1 contents1) (Elem qname2 attrs2 contents2) | qname1==qname2 =
	Element qname1 (mergeList sameAttribute mergeAttributes attrs1 attrs2) (mergeList sameContent mergeContents contents1 contents2)

mergeList :: (a -> a -> Bool) -> (a -> a -> a) -> [a] -> [a] -> [a]
mergeList same_elem merge_elem (elem1:rest1) (elem2:rest2) | same_elem elem1 elem2 =
	merge_elem elem1 elem2 : mergeList rest1 rest2
mergeList 

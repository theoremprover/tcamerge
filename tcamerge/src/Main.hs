{-# LANGUAGE ScopedTypeVariables #-}

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
    let [mergedelement] = mergeElements [base] [mod1]
	writeFile ("merged_"++filepath) $ document $ render $ embedmod1 mergedelement

mergeElements :: [Element i] -> [Element i] -> [Element i]
mergeElements elem1 [] = [elem1]
mergeElements [] elem2 = [elem2]
mergeElements (Elem qname1 attrs1 contents1 : rest1) (Elem qname2 attrs2 contents2 : rest2) | qname1==qname2 =
	Element qname1 (mergeList attrs1 attrs2) (mergeList contents1 contents2) : mergeElements rest1 rest2
mergeElement elem1 elem2 = 

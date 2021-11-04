{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE ScopedTypeVariables,FlexibleInstances,UnicodeSyntax #-}

module Main where

import Text.XML.Light
import Data.List

type XML = [Content]

readXML :: FilePath -> IO XML
readXML filepath = do
	content <- readFile filepath
	return $ parseXML content

writeXML :: FilePath -> [Content] -> IO ()
writeXML filepath [Elem element] = do
	writeFile filepath $ ppTopElement element

main :: IO ()
main = do
	xml1 <- readXML "test1.xml"
	xml2 <- readXML "test2.xml"
	print $ mergeXML xml1 xml2

mergeXML :: XML -> XML -> XML
mergeXML xml1 xml2 =
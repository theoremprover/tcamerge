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
	xml1 <- readXML "Branch.tca"
	xml2 <- readXML "test2.xml"
	putStrLn $ ppTopElement $ mergeXML xml1 xml2
--	writeXML "testout.xml" 

mergeXML :: XML -> XML -> XML
mergeXML xml1 xml2 = xml1

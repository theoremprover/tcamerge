{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE ScopedTypeVariables,FlexibleInstances,UnicodeSyntax #-}

-- https://hackage.haskell.org/package/xml-1.3.14/docs/Text-XML-Light-Types.html#t:CData

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
	let [res_xml] = diff xml1 xml2
	writeXML "merged.tca" res_xml	


---- Same ----------------------	
-- Determines if something should be considered as the same element when merging

class Same a where
	same :: a -> a -> Bool

instance Same Element where
	same (Element name1 attribs1 contents1 line1) (Element name2 attribs2 contents2 _) =
		name1==name2 && idAttrib attribs1 == idAttrib attribs2
		where
		idAttrib attribs = case filter ((=="id").qName.attrKey) attribs of
			[Attr key val] -> Just val
			_              -> Nothing

instance Same Attr where
	same (Attr qname1 _) (Attr qname2 _) = qname1==qname2

---- Diff Tags ------------------


---- Diffable -------------------

class Same a => Diffable a where
	diff :: a -> a -> [a]
	mkInserted :: a -> a
	mkDeleted  :: a -> a
	isInserted :: a -> Bool
	isDeleted  :: a -> Bool

{--
diffList [A,B] [A,C,B] = [ A, Inserted C, B ]
diffList [A,B,C] [B,C] = [ Deleted A, B ]
diffList [A] [B] = [ Deleted A, Inserted B ]
--}
diffList :: (Diffable a) => [a] -> [a] -> [a]
diffList l1 [] = l1
diffList [] l2 = l2
diffList (e1:l1s) (e2:l2s) | e1 `same` e2 = diff e1 e2 ++ diffList l1s l2s
diffList l1@(e1:l1s) l2@(e2:l2s) = case (collect_diffs_and_rest e1 l2s,collect_diffs_and_rest e2 l1s) of
	((inserted,rest),(_,[])) | not (null inserted) -> map mkInserted inserted ++ diffList l1 rest
	((_,[]),(deleted,rest))  | not (null deleted)  -> map mkDeleted  deleted  ++ diffList l2 rest
	((_,[]),(_,[]))                                -> mkDeleted e1 : mkInserted e2 : diffList l1s l2s
	where
	collect_diffs_and_rest :: Same a => a -> [a] -> ([a],[a])
	collect_diffs_and_rest e l = span (not.(same e)) l

insertedQName = QName "_INSERTED_" Nothing Nothing
deletedQName  = QName "_DELETED_"  Nothing Nothing
instance Diffable Element where
	diff elem1@(Element name1 attribs1 contents1 line1) elem2@(Element name2 attribs2 contents2 _)
		| elem1 `same` elem2 = [ Element name1 (diffList attribs1 attribs2) (diffList contents1 contents2) line1 ]
	diff elem1 elem2 = []
	mkInserted elem = Element insertedQName [] [Elem elem] Nothing
	mkDeleted elem  = Element deletedQName  [] [Elem elem] Nothing
	isInserted = (==insertedQName).elName
	isDeleted  = (==deletedQName).elName

instance Diffable Content where
	diff (Elem elem1) (Elem elem2) = map Elem $ diff elem1 elem2
	diff (Text (CData CDataText text1 _)) (Text (CData CDataText text2 _)) = map () $ diff text1 text2

instance Diffable Attr where

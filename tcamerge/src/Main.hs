{-# LANGUAGE ScopedTypeVariables #-}

-- stack exec DtdToHaskell -- tca.dtd >src\Extsubset.hs

module Main where

--import Text.XML.HaXml
import Text.XML.HaXml.XmlContent

import Extsubset

xmlFileName = "Branch.tca"

main :: IO ()
main = do
    a :: Xmi'XMI <- fReadXml xmlFileName
    print a

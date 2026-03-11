#!/usr/bin/runhaskell

import Text.XML.HXT.Core
import Control.Monad

main :: IO ()
main = do
    [doc] <- runX $ readDocument [] "data/xhtml5.xsd"
    let elts = run doc $ this //> hasName "xs:element"
            >>> (getAttrValue0 "name" &&& this)
    forM_ elts $ \(name, node) ->
        processElement doc name node

processElement :: XmlTree -> String -> XmlTree -> IO ()
processElement doc name node = do
    let uattrs = run node $ this //> hasName "xs:attribute"
            >>> hasAttrValue "type" (== "uri") >>> getAttrValue0 "name"
    forM_ uattrs $ \attrb ->
        putStrLn $ name ++ " " ++ attrb

    let attrgs = run node $ this //> hasName "xs:attributeGroup"
            >>> getAttrValue0 "ref"
    forM_ attrgs $ \attrg -> do
        let l = run doc $ this //> hasName "xs:attributeGroup"
                >>> hasAttrValue "name" (== attrg)
        mapM_ (processElement doc name) l

    let typs = run node $ getAttrValue0 "type"
    forM_ typs $ \typ -> do
        let ctypes = run doc $ this //> hasName "xs:complexType"
                >>> hasAttrValue "name" (== typ)
        mapM_ (processElement doc name) ctypes

run :: a -> LA a b -> [b]
run = flip runLA

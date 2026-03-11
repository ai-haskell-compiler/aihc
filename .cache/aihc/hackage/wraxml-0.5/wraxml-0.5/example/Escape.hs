{- |
Detects character encoding, decodes,
and convert all special characters to HTML references.
CDATA sections are converted to plain text.
Input is read from standard input,
output is written to standard output.
The encoding can be given as one argument,
which then overrides encoding information within the document.
Since an HTML tree is used as internal representation,
in the output opening and closing tags are correctly nested.
This may be an improvement,
but if the input is written with bugs or strange behaviour
of a particular HTML viewer in mind,
this may be a loss.

> escape-tidy-html utf-8 <input.xhtml >output.xhtml

See also @escape-html@ example from tagchup package.
-}
module Main where

import qualified Text.XML.HXT.DOM.Unicode as Unicode

import qualified Text.HTML.WraXML.Document as HtmlDoc
import qualified Text.XML.WraXML.Document.Tagchup as TagchupDoc
import qualified Text.XML.WraXML.Tree as XmlTree
import qualified Text.XML.WraXML.String as XmlString
import qualified Text.HTML.Tagchup.Parser as Parser
import qualified Text.HTML.Basic.Character as HtmlChar
import qualified Text.XML.Basic.Name.LowerCase as Name

import System.Environment (getArgs, )



getDecoder :: XmlString.Encoding -> XmlString.Encoded -> String
getDecoder =
   maybe Unicode.latin1ToUnicode (fst.) .
   Unicode.getDecodingFct

escape :: HtmlDoc.T i Name.T String -> String
escape =
   flip HtmlDoc.format "" .
   fmap (map HtmlChar.asciiFromUnicode) .
   HtmlDoc.lift id (map XmlTree.textFromCData)

main :: IO ()
main =
   do argv <- getArgs
      case argv of
         [] ->
            interact
               (escape .
                XmlString.evalDecodeAdaptive .
                HtmlDoc.decodeAdaptive getDecoder .
                TagchupDoc.toXmlDocument .
                Parser.runSoupWithPositions)
         [encoding] ->
            interact
               (escape .
                TagchupDoc.toXmlDocument .
                Parser.runSoupWithPositions .
                getDecoder encoding)
         _ ->
            putStrLn "Error: more than one decoding given"

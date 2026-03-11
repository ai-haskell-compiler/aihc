{- |
Detects character encoding, decodes,
and convert all special characters to HTML references.
CDATA sections are converted to plain text.
Input is read from standard input,
output is written to standard output.
The encoding can be given as one argument,
which then overrides encoding information within the document.

> escape-html utf-8 <input.xhtml >output.xhtml
-}
module Main where

import qualified Text.XML.HXT.DOM.Unicode as Unicode

import qualified Text.HTML.Tagchup.Parser as Parser
import qualified Text.HTML.Tagchup.Format as Format
import qualified Text.HTML.Tagchup.Process as Process
import qualified Text.HTML.Tagchup.Tag     as Tag
import qualified Text.HTML.Basic.Character as HTMLChar
import qualified Text.XML.Basic.Name.LowerCase as Name

import System.Environment (getArgs, )


getDecoder :: Process.Encoding -> Process.Encoded -> String
getDecoder =
   maybe Unicode.latin1ToUnicode (fst.) .
   Unicode.getDecodingFct

escape :: [Tag.T Name.T String] -> String
escape =
   flip Format.htmlOrXhtml "" .
   map (fmap (map HTMLChar.asciiFromUnicode) . Tag.textFromCData)

main :: IO ()
main =
   do argv <- getArgs
      case argv of
         [] ->
            interact
               (escape .
                Process.evalDecodeAdaptive .
                Process.decodeAdaptive getDecoder .
                Parser.runSoup)
         [encoding] ->
            interact
               (escape .
                Parser.runSoup .
                getDecoder encoding)
         _ ->
            putStrLn "Error: more than one decoding given"

{- |
Strip HTML comments, script and style tags.

> strip-html <input.xhtml >output.xhtml
-}
module Main where

import qualified Text.HTML.Tagchup.Parser as Parser
import qualified Text.HTML.Tagchup.Format as Format
import qualified Text.HTML.Tagchup.Process as Process
import qualified Text.HTML.Tagchup.Tag     as Tag
import qualified Text.XML.Basic.Name.LowerCase as NameLC
import qualified Text.XML.Basic.Name as Name


strip :: [Tag.T NameLC.T String] -> String
strip =
   flip Format.htmlOrXhtml "" .
   concatMap (either (const []) id) .
   Process.parts (Name.matchAny ["script", "style"]) .
   filter (not . Tag.isComment)

main :: IO ()
main =
   interact (strip . Parser.runSoup)

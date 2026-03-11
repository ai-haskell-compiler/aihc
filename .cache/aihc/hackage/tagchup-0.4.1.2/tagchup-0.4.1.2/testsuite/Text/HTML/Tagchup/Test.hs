module Text.HTML.Tagchup.Test (
    -- * Tests on laziness
    laziness, lazyTags, lazyWarnings, restrictedWarnings,
   ) where

import qualified Text.HTML.Tagchup.Tag    as Tag
import qualified Text.HTML.Tagchup.Parser as Parser
import qualified Text.XML.Basic.Name.MixedCase as Name
import Data.List (isPrefixOf, )

{-
*Text.HTML.TagSoup.HT> mapM print $ runSoup $ "</html " ++ cycle " abc=a_b&c"
("anonymous input" (line 1, column 1),TagClose "html")
("anonymous input" (line 1, column 9),TagWarning "Junk in closing tag: \"abc=a_b&c abc=a_b&c abc=a_b&c abc=a_b&c abc=a_b&c abc=a_b&c abc=a_b&c abc=a_b&c abc=a_b&c abc=a_b&c abc=a_b&c abc=a_b&c abc=a_b&c abc=a
-}

{- |
This routine tests the laziness of the TagSoup parser.
For each critical part of the parser we provide a test input
with a token of infinite size.
Then the output must be infinite too.
If the laziness is broken, then the output will stop early.
We collect the thousandth character of the output of each test case.
If computation of the list stops somewhere,
you have found a laziness stopper.
-}
laziness :: [Char]
laziness = lazyTags ++ lazyWarnings ++ restrictedWarnings

runSoup :: String -> [Tag.T Name.T String]
runSoup = Parser.runSoup

lazyTags :: [Char]
lazyTags =
   map ((!!1000) . show . runSoup) $
      (cycle "Rhabarber") :
      (repeat '&') :
      ('&' : '#' : repeat '1') :
      ('&' : repeat 'a') :
      ("<"++cycle "html") :
      ("<html "++cycle "name") :
      ("<html "++cycle "na!me=value ") :
      ("<html name="++cycle "value") :
      ("<html name=\""++cycle "value") :
      ("<html name="++cycle "val!ue") :
      ("<html name="++cycle "val&ue") :
      ("<html name="++cycle "va&l!ue") :
      ("</"++cycle "html") :
      ("</html "++cycle "junk") :
      ("<!-- "++cycle "comment") :
      ("<!"++cycle "doctype") :
      ("<![CDATA["++cycle "content&am<") :
      ("<!DOCTYPE"++cycle " description") :
      ("<?xml "++cycle "name") :
      ("<?custom "++cycle "name") :
      (cycle "1<2 ") :
      []

lazyWarnings :: [Char]
lazyWarnings =
   map ((!!1000) . show . tail . runSoup) $
      (repeat '&') :
      ("<html "++cycle "na!me=value ") :
      ("</html "++cycle "junk") :
      (cycle "1<2 ") :
      []

restrictedWarnings :: [Char]
restrictedWarnings =
   map (last . show . head .
        dropWhile (not . maybe False (isPrefixOf "further") . Tag.maybeWarning) .
        runSoup) $
      ("<html name="++cycle "val!ue") :
      ("<html name="++cycle "val&ue") :
      ("<html name="++cycle "va&l!ue") :
      []

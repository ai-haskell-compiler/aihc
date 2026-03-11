{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Text.XML.Expat.ParseFormat where

import Text.XML.Expat.Extended
import Text.XML.Expat.Format
import qualified Text.XML.Expat.Tree as Tree
import qualified Text.XML.Expat.Annotated as Annotated

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit


tests = hUnitTestToTests $
    TestList $ concatMap mkTests pfTests

pfTests :: [PFTest]
pfTests = [
        PFTest {
            pfName = "quotation",
            pfXML = "<test>\n" `mappend`
                    "<sample id=\"5\">This \"text with quotations\" should be escaped.</sample>\n" `mappend`
                    "<mytest>\n" `mappend`
                    "//<![CDATA[\n" `mappend`
                    "This \"text with quotations\" should not be escaped.\n" `mappend`
                    "Another line goes here.\n" `mappend`
                    "\n" `mappend`
                    "And more.\n" `mappend`
                    "//]]>\n" `mappend`
                    "</mytest>\n" `mappend`
                    "<?php somecode(); ?>\n" `mappend`
                    "<!-- this is a comment -->\n" `mappend`
                    "</test>",
            pfDoc = mkPlainDocument $
                Element "test" [] [
                    Text "\n",
                    Element "sample" [("id","5")] [Text "This \"text with quotations\" should be escaped."] (),
                    Text "\n",
                    Element "mytest" [] [
                        Text "\n",
                        Text "//",
                        CData "\nThis \"text with quotations\" should not be escaped.\nAnother line goes here.\n\nAnd more.\n//",
                        Text "\n"
                    ] (),
                    Text "\n",
                    Misc (ProcessingInstruction "php" "somecode(); "),
                    Text "\n",
                    Misc (Comment " this is a comment "),
                    Text "\n"
                ] (),
            pfOutXML = [(Extended,
                    "<test>\n" `mappend`
                    -- " gets translated into &quot; here but not inside CDATA.
                    "<sample id=\"5\">This &quot;text with quotations&quot; should be escaped.</sample>\n" `mappend`
                    "<mytest>\n" `mappend`
                    "//<![CDATA[\n" `mappend`
                    "This \"text with quotations\" should not be escaped.\n" `mappend`
                    "Another line goes here.\n" `mappend`
                    "\n" `mappend`
                    "And more.\n" `mappend`
                    "//]]>\n" `mappend`
                    "</mytest>\n" `mappend`
                    "<?php somecode(); ?>\n" `mappend`
                    "<!-- this is a comment -->\n" `mappend`
                    "</test>"                )],
            pfImpls = [Extended]
        },
        PFTest {
            pfName = "xmlDecl1",
            pfXML = "<?xml version=\"1.0\"?>\n<hello/>",
            pfDoc = Document (Just (XMLDeclaration "1.0" Nothing Nothing)) Nothing [] (Element "hello" [] [] ()),
            pfOutXML = [],
            pfImpls = [Extended]
        },
        PFTest {
            pfName = "xmlDecl2",
            pfXML = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n<hello/>",
            pfDoc = Document (Just (XMLDeclaration "1.0" (Just "ISO-8859-1") Nothing)) Nothing [] (Element "hello" [] [] ()),
            pfOutXML = [],
            pfImpls = [Extended]
        },
        PFTest {
            pfName = "xmlDecl3",
            pfXML = "<?xml version=\"1.0\" standalone=\"yes\"?>\n<hello/>",
            pfDoc = Document (Just (XMLDeclaration "1.0" Nothing (Just True))) Nothing [] (Element "hello" [] [] ()),
            pfOutXML = [],
            pfImpls = [Extended]
        },
        PFTest {
            pfName = "xmlDecl4",
            pfXML = "<?xml version=\"1.0\" standalone=\"no\"?>\n<hello/>",
            pfDoc = Document (Just (XMLDeclaration "1.0" Nothing (Just False))) Nothing [] (Element "hello" [] [] ()),
            pfOutXML = [],
            pfImpls = [Extended]
        },
        PFTest {
            pfName = "topLevelMiscs1",
            pfXML = "<?xml version=\"1.0\"?>\n<?process My code?>\n<!-- And a comment -->\n<hello/>",
            pfDoc = Document (Just (XMLDeclaration "1.0" Nothing Nothing)) Nothing [
                    ProcessingInstruction "process" "My code",
                    Comment " And a comment "
                ] (Element "hello" [] [] ()),
            pfOutXML = [],
            pfImpls = [Extended]
        },
        PFTest {
            pfName = "topLevelMiscs2",
            -- Test that we can read processing instructions and comments from after the root element.
            pfXML = "<?xml version=\"1.0\"?>\n<?process My code?>\n<!-- And a comment -->\n" `mappend`
                    "<hello/>\n<!-- Also afterwards -->\n<?php something();?>",
            pfDoc = Document (Just (XMLDeclaration "1.0" Nothing Nothing)) Nothing [
                    ProcessingInstruction "process" "My code",
                    Comment " And a comment ",
                    Comment " Also afterwards ",
                    ProcessingInstruction "php" "something();"
                ] (Element "hello" [] [] ()),
            -- In the output they appear *before* the root element, however.
            pfOutXML = [(Extended,
                    "<?xml version=\"1.0\"?>\n<?process My code?>\n<!-- And a comment -->\n" `mappend`
                    "<!-- Also afterwards -->\n<?php something();?>\n<hello/>"
                )],
            pfImpls = [Extended]
        },
        PFTest {
            pfName = "basic",
            pfXML = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" `mappend`
                    "<second><test><test1 type=\"expression\">Cat &amp; mouse</test1>In between" `mappend`
                    "<test2 type=\"communication\" language=\"Rhyming slang\">Dog &amp; bone</test2>" `mappend`
                    "</test><test>Rose &amp; Crown</test></second>",
            pfDoc = Document (Just (XMLDeclaration "1.0" (Just "UTF-8") Nothing)) Nothing [] (
                 Element "second" [] [Element "test" [] [Element "test1" [("type","expression")]
                     [Text "Cat ",Text "&",Text " mouse"] (),Text "In between",
                      Element "test2" [("type","communication"),("language","Rhyming slang")]
                     [Text "Dog &",Text " bone"] ()] (),Element "test" []
                     [Text "Ro", Text "se & Crown"] ()] ()),  -- Test text normalization
            pfOutXML = [],
            pfImpls = [Tree, Annotated, Extended]
        },
        PFTest {
            pfName = "escaping of >",
            pfXML = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<text>]]&gt;</text>",
            pfDoc = Document (Just (XMLDeclaration "1.0" (Just "UTF-8") Nothing)) Nothing [] (
                Element "text" [] [Text "]]>"] ()),
            pfOutXML = [],
            pfImpls = [Extended]
        }
    ]

-- | Recursively append all adjacent Text nodes.
normalizeText :: (NodeClass n [], Monoid text) => n [] tag text -> n [] tag text
normalizeText = modifyChildren combine
  where
    combine (t1:t2:ns) | isText t1 && isText t2 = combine ((mkText $ getText t1 `mappend` getText t2):ns)
    combine (e:ns) | isElement e = normalizeText e : combine ns
    combine (n:ns) = n:combine ns
    combine [] = []

mkTests :: PFTest -> [Test]
mkTests pf = flip concatMap (pfImpls pf) $ \impl ->
    case impl of
        Tree -> [
                TestLabel (pfName pf ++ "-tree") $ TestCase $ do
                    case Tree.parse' defaultParseOptions (pfXML pf) of
                        Left err -> assertFailure $ "parse failed: "++show err
                        Right root0 -> do
                            let root = normalizeText root0
                                sbDoc = normalizeText $ fromElement (getRoot $ pfDoc pf)
                            assertEqual "parse match" sbDoc (root :: Tree.UNode Text)
                            let sb = fromMaybe (pfXML pf) (impl `lookup` pfOutXML pf)
                                bs = format' root
                            assertEqual "format match" sb bs
            ]
        Annotated -> [
                TestLabel (pfName pf ++ "-tree") $ TestCase $ do
                    case Annotated.parse' defaultParseOptions (pfXML pf) of
                        Left err -> assertFailure $ "parse failed: "++show err
                        Right root0 -> do
                            let root = normalizeText $ Annotated.mapAnnotation (const ()) root0
                                sbDoc = normalizeText $ fromElement (getRoot $ pfDoc pf)
                            assertEqual "parse match" sbDoc (root :: Annotated.UNode () Text)
                            let sb = fromMaybe (pfXML pf) (impl `lookup` pfOutXML pf)
                                bs = format' root
                            assertEqual "format match" sb bs
            ]
        Extended  -> [
                TestLabel (pfName pf ++ "-extended") $ TestCase $ do
                    case parse' defaultParseOptions (pfXML pf) of
                        Left err -> assertFailure $ "parse failed: "++show err
                        Right doc0 -> do
                            let doc = modifyRoot normalizeText $ mapDocumentAnnotation (const ()) doc0
                            assertEqual "parse match" (modifyRoot normalizeText $ pfDoc pf) doc
                    let sb = fromMaybe (pfXML pf) (impl `lookup` pfOutXML pf)
                        bs = formatDocument' (pfDoc pf)
                    assertEqual "format match" sb bs
            ]

data Impl = Tree | Annotated | Extended deriving (Eq, Ord, Show)

data PFTest = PFTest {
        pfName   :: String,
        pfXML    :: ByteString,
        pfDoc    :: UDocument () Text,
        pfOutXML :: [(Impl, ByteString)],  -- ^ Output XML where it differs from the input XML
        pfImpls  :: [Impl]
    }


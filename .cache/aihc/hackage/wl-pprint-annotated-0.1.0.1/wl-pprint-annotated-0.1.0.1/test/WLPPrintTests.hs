module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Text.PrettyPrint.Annotated.WL

main :: IO ()
main = defaultMain $ testGroup "Tests" [
         testGroup "Tests for each data constructor" conTests
       , testGroup "Tests for some combinators" codeTests
       , testGroup "Tests for the formatting algorithms" formatTests
       , testGroup "Tests for the code examples in the documentation" docTests
       ]

conTests :: [TestTree]
conTests = [
    testCase "Empty tests"   emptyTests
  , testCase "Char tests"    charTests
  , testCase "Text tests"    textTests
  , testCase "Line tests"    textTests
  , testCase "FlatAlt tests" flatAltTests
  , testCase "Cat tests"     catTests
  , testCase "Nest tests"    nestTests
  , testCase "Union tests"   unionTests
  , testCase "Column tests"  columnTests
  , testCase "Nesting tests" nestingTests
  , testCase "Columns tests" columnsTests
  , testCase "Ribbon tests"  ribbonTests
  ]

------------------------------------------------------------
-- Helper to add annotations

annotateAll :: Doc a -> Doc ()
annotateAll = Annotate () . go
  where go Empty          = Empty
        go (Char x)       = Char x
        go (Text i s)     = Text i s
        go Line           = Line
        go (FlatAlt l r)  = FlatAlt (annotateAll l) (annotateAll r)
        go (Cat l r)      = Cat (annotateAll l) (annotateAll r)
        go (Nest i d)     = Nest i (annotateAll d)
        go (Union l r)    = Union (annotateAll l) (annotateAll r)
        go (Annotate _ d) = annotateAll d
        go (Column f)     = Column (annotateAll . f)
        go (Nesting k)    = Nesting (annotateAll . k)
        go (Columns k)    = Columns (annotateAll . k)
        go (Ribbon k)     = Ribbon (annotateAll . k)

------------------------------------------------------------
-- We test the @Doc@ constructors.

assertPretty :: Int -> String -> String -> Doc a -> Assertion
assertPretty w desc str doc = do
  assertEqual (desc ++ " (pretty)") str
    $ displayS (renderPretty 1.0 w doc) ""
  assertEqual (desc ++ " (pretty, annotated)") str
    $ displayS (renderPretty 1.0 w $ annotateAll doc) ""

assertSmart :: Int -> String -> String -> Doc a -> Assertion
assertSmart w desc str doc = do
  assertEqual (desc ++ " (smart)") str
    $ displayS (renderSmart w doc) ""
  assertEqual (desc ++ " (smart, annotated)") str
    $ displayS (renderSmart w $ annotateAll doc) ""

assertRender :: Int -> String -> String -> Doc a -> Assertion
assertRender w desc str doc = do assertPretty w desc str doc
                                 assertSmart w desc str doc

emptyTests :: Assertion
emptyTests = assertRender 80 "Empty test 1" "" mempty

charTests :: Assertion
charTests = assertRender 80 "Char test 1" "a" (char 'a')

textTests :: Assertion
textTests = assertRender 80 "Text test 1" "text..." (text "text...")

lineTests :: Assertion
lineTests = assertRender 80 "Line test 1" "\n" hardline

flatAltTests :: Assertion
flatAltTests = do assertRender 80 "FlatAlt test 1" "x"
                    $ flatAlt (text "x") (text "y")
                  assertRender 80 "FlatAlt test 2" "y"
                    $ flatten $ flatAlt (text "x") (text "y")

catTests :: Assertion
catTests = assertRender 80 "Cat test 1" "some code"
                $ text "some" <> space <> text "code"

nestTests :: Assertion
nestTests = do assertRender 80 "Nest test 1" "foo bar"
                 $ text "foo" <+> nest 2 (text "bar")
               assertRender 80 "Nest test 2" "foo\n  bar"
                 $ text "foo" <> nest 2 (line <> text "bar")

unionTests :: Assertion
unionTests = do assertRender 80 "Union test 1" "foo bar"
                  $ text "foo" </> text "bar"
                assertRender 5 "Union test 2" "foo\nbar"
                  $ text "foo" </> text "bar"

columnTests :: Assertion
columnTests = assertRender 80 "Column test 1" "foo 4"
                 $ text "foo" <+> column pretty

nestingTests :: Assertion
nestingTests =  assertRender 80 "Nesting test 1" "foo 2"
                  $ text "foo" <+> nest 2 (nesting pretty)

columnsTests :: Assertion
columnsTests = assertRender 21 "Columns test 1" "foo 21"
                  $ text "foo" <+> nest 2 (columns pretty)

ribbonTests :: Assertion
ribbonTests = assertEqual "Ribbon test 1" "foo 40"
                 $ show (text "foo" <+> ribbon pretty)

------------------------------------------------------------
-- We test some combinators.

codeTests :: [TestTree]
codeTests = [
    testCase "@list@ tests"   listTests
  , testCase "@tupled@ tests" tupledTests
  ]

listTests :: Assertion
listTests = do assertRender 80 "@list@ test 1" "[1, 2, 3]"
                 $ pretty ([1, 2, 3] :: [Int])
               assertRender 5 "@list@ test 2" "[ 1\n, 2\n, 3 ]"
                 $ pretty ([1, 2, 3] :: [Int])

tupledTests :: Assertion
tupledTests = do assertRender 80 "@tupled@ test 1" "(1, True, a)"
                   $ pretty (1 :: Int, True, 'a')
                 assertRender 5 "@tupled@ test 2" "( 1\n, True\n, a )"
                   $ pretty (1 :: Int, True, 'a')

------------------------------------------------------------
-- We test some corner cases of the formatting algorithms on a prototypical
-- syntax for a scripting language (e.g. Python).

formatTests :: [TestTree]
formatTests = [
    testCase "@renderPretty@ test" renderPrettyTest
  , testCase "@renderSmart@ test"  renderSmartTest
  ]

data Syn = Call String [Syn]    -- name(syn, ..., syn)
         | Plus Syn Syn  -- syn + syn
         | List [Syn]    -- [syn, ..., syn]
         | Name String

instance Pretty Syn where
  pretty (Call n s) =
    text (n ++ "(") <> nest 2 (softbreak <> align body) <//> rparen
    where body = hcat $ punctuate (comma <> softline) (map pretty s)
  pretty (Plus s t) = nest 2 (pretty s) </> text "+" </> nest 2 (pretty t)
  pretty (List l) = list (map pretty l)
  pretty (Name n) = text n

deep :: Int -> Syn
deep 0 = List (map Name ["abc", "abcdef", "abcdef"])
deep i = Call "fun" [deep (i - 1)]

branch :: Int -> Syn
branch 0 = List (map Name ["abc", "abc"])
branch i | i < 3 = Call "fun" [branch (i - 1), branch (i - 1)]
         | otherwise = Call "fun" [branch (i - 1)]

wide :: Syn
wide = Call "aaaaaaaaaaa" [List $ map Name ["abc", "def", "ghi"]]

-- @renderPretty@ has only one line of lookahead, so it can not fit the
-- entire document within the pagewidth (20c), only the first line.
renderPrettyTest :: Assertion
renderPrettyTest = do assertPretty 20 "@renderPretty@ test 1" (concat [
                          "fun(fun(fun(fun(fun(\n"
                        , "                  [ abc\n"
                        , "                  , abcdef\n"
                        , "                  , abcdef ]\n"
                        , "                ))))\n"
                        , ")" ])
                        $ pretty (deep 5)
                      assertPretty 20 "@renderPretty@ test 2" (concat [
                          "fun(fun(fun([ abc\n"
                        , "            , abc ],\n"
                        , "            [ abc\n"
                        , "            , abc ]\n"
                        , "        ), fun([ abc\n"
                        , "               , abc ],\n"
                        , "               [ abc\n"
                        , "               , abc ]\n"
                        , "        )))" ])
                        $ pretty (branch 3)
                      assertPretty 20 "@renderPretty@ test 3" (concat [
                          "aaaaaaaaaaa([ abc\n"
                        , "            , def\n"
                        , "            , ghi ])" ])
                        $ pretty wide

-- @renderSmart@ has more sophisiticated lookahead, so it fits the entire
-- structure within the pagewidth (20c).
renderSmartTest :: Assertion
renderSmartTest = do assertSmart 20 "@renderSmart@ test 1" (concat [
                         "fun(\n"
                       , "  fun(\n"
                       , "    fun(\n"
                       , "      fun(\n"
                       , "        fun(\n"
                       , "          [ abc\n"
                       , "          , abcdef\n"
                       , "          , abcdef ]\n"
                       , "        )))))" ])
                       $ pretty (deep 5)
                     assertSmart 20 "@renderSmart@ test 2" (concat [
                         "fun(\n"
                       , "  fun(\n"
                       , "    fun(\n"
                       , "      fun(\n"
                       , "        fun([ abc\n"
                       , "            , abc ],\n"
                       , "            [ abc\n"
                       , "            , abc ]\n"
                       , "        ),\n"
                       , "        fun([ abc\n"
                       , "            , abc ],\n"
                       , "            [ abc\n"
                       , "            , abc ])\n"
                       , "      ))))" ])
                       $ pretty (branch 5)
                     assertSmart 20 "@renderSmart@ test 3" (concat [
                         "aaaaaaaaaaa(\n"
                       , "  [abc, def, ghi])" ])
                       $ pretty wide

------------------------------------------------------------
-- We test the code examples in the haddock comments.

docTests :: [TestTree]
docTests = [
    testCase "@fill@ test" fillTest
  , testCase "@fillBreak@ test" fillBreakTest
  , testCase "@hang@ test" hangTest
  , testCase "@align@ test" alignTest
  ]

types :: [(String, String)]
types = [ ("empty","Doc e")
        , ("nest","Int -> Doc e -> Doc e")
        , ("linebreak","Doc e") ]

fillTest :: Assertion
fillTest = assertRender 80 "@fill@ test 1" (concat [
                  "let empty  :: Doc e\n"
                , "    nest   :: Int -> Doc e -> Doc e\n"
                , "    linebreak :: Doc e" ])
                $ text "let" <+> align (vcat (map ptype types))
              where ptype (name, tp) =
                      fill 6 (text name) <+> text "::" <+> text tp

fillBreakTest :: Assertion
fillBreakTest = assertRender 80 "@fillBreak@ test 1" (concat [
                       "let empty  :: Doc e\n"
                     , "    nest   :: Int -> Doc e -> Doc e\n"
                     , "    linebreak\n"
                     , "           :: Doc e" ])
                     $ text "let" <+> align (vcat (map ptype types))
                where ptype (name, tp) =
                        fillBreak 6 (text name) <+> (text "::" <+> text tp)

hangTest :: Assertion
hangTest = assertRender 20 "@hang@ test 1" (concat [
                  "the hang combinator\n"
                , "    indents these\n"
                , "    words !" ])
                $ hang 4 $ fillSep $ map text
                $ words "the hang combinator indents these words !"

alignTest :: Assertion
alignTest = assertRender 20 "@align@ test 1" (concat [
                   "hi nice\n"
                 , "   world" ])
                 $ text "hi" <+> (text "nice" $$ text "world")
               where x $$ y = align $ x <> linebreak <> y

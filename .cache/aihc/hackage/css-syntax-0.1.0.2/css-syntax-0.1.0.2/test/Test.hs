{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Main where


import qualified Data.Text as T
import           Data.Monoid
import           Data.CSS.Syntax.Tokens
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Prelude
import           Test.QuickCheck
import           Data.Scientific

testTokenize :: HasCallStack => T.Text -> [Token] -> Expectation
testTokenize s t = do
    tokenize s `shouldBe` t
    tokenize (serialize (tokenize s)) `shouldBe` t

testSerialize :: HasCallStack => [Token] -> T.Text -> Expectation
testSerialize t s = do
    serialize t `shouldBe` s
    serialize (tokenize (serialize t)) `shouldBe` s


main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do

    describe "Data.CSS.Syntax.Tokens" $ do
        it "Single Character" $ do
            testTokenize "(" [LeftParen]
            testTokenize ")" [RightParen]
            testTokenize "[" [LeftSquareBracket]
            testTokenize "]" [RightSquareBracket]
            testTokenize ",," [Comma, Comma]

        it "Multiple Character" $ do
            testTokenize "~=" [IncludeMatch]
            testTokenize "||" [Column]
            testTokenize "|||" [Column, Delim '|']
            testTokenize "<!--" [CDO]
            testTokenize "<!---" [CDO, Delim '-']
            testTokenize "<!---->" [CDO, CDC]
            testTokenize "<!-- -->" [CDO, Whitespace, CDC]

        it "Delimiter" $ do
            testTokenize "^" [Delim '^']
            testTokenize "|" [Delim '|']
            testTokenize "\x7f" [Delim '\x7f']
            testTokenize "\1" [Delim '\x1']
            testTokenize "~-" [Delim '~', Delim '-']
            testTokenize "*^" [Delim '*', Delim '^']

        it "Whitespace" $ do
            testTokenize "     " [Whitespace]
            testTokenize "\n\rS" [Whitespace, Ident "S"]
            testTokenize "    *" [Whitespace, Delim '*']
            testTokenize "\n\r\f2" [Whitespace, Number "2" (NVInteger 2)]

        it "Escapes" $ do
            testTokenize "hel\\6Co" [Ident "hello"]
            testTokenize "\\26 B" [Ident "&B"]
            testTokenize "'hel\\6c o'" [String "hello"]
            testTokenize "'spac\\65\r\ns'" [String "spaces"]
            testTokenize "spac\\65\r\ns" [Ident "spaces"]
            testTokenize "spac\\65\n\rs" [Ident "space", Whitespace, Ident "s"]
            testTokenize "sp\\61\tc\\65\fs" [Ident "spaces"]
            testTokenize "hel\\6c  o" [Ident "hell", Whitespace, Ident "o"]
            testTokenize "test\\\n" [Ident "test", Delim '\\', Whitespace]
            testTokenize "test\\D799" [Ident "test\xD799"]
            testTokenize "\\E000" [Ident "\xe000"]
            testTokenize "te\\s\\t" [Ident "test"]
            testTokenize "spaces\\ in\\\tident" [Ident "spaces in\tident"]
            testTokenize "\\.\\,\\:\\!" [Ident ".,:!"]
            testTokenize "\\\r" [Delim '\\', Whitespace]
            testTokenize "\\\f" [Delim '\\', Whitespace]
            testTokenize "\\\r\n" [Delim '\\', Whitespace]
            -- let replacement = "\xFFFD"
            testTokenize "null\\\0" [Ident "null\xfffd"]
            testTokenize "null\\\0\0" [Ident $ "null" <> "\xfffd" <> "\xfffd"]
            testTokenize "null\\0" [Ident $ "null" <> "\xfffd"]
            testTokenize "null\\0000" [Ident $ "null" <> "\xfffd"]
            testTokenize "large\\110000" [Ident $ "large" <> "\xfffd"]
            testTokenize "large\\23456a" [Ident $ "large" <> "\xfffd"]
            testTokenize "surrogate\\D800" [Ident $ "surrogate" <> "\xfffd"]
            testTokenize "surrogate\\0DABC" [Ident $ "surrogate" <> "\xfffd"]
            testTokenize "\\00DFFFsurrogate" [Ident $ "\xfffd" <> "surrogate"]
            testTokenize "\\10fFfF" [Ident "\x10ffff"]
            testTokenize "\\10fFfF0" [Ident $ "\x10ffff" <> "0"]
            testTokenize "\\10000000" [Ident $ "\x100000" <> "00"]
            testTokenize "eof\\" [Ident "eof", Delim '\\']

        it "Ident" $ do
            testTokenize "simple-ident" [Ident "simple-ident"]
            testTokenize "testing123" [Ident "testing123"]
            testTokenize "hello!" [Ident "hello", Delim '!']
            testTokenize "world\5" [Ident "world", Delim '\5']
            testTokenize "_under score" [Ident "_under", Whitespace, Ident "score"]
            testTokenize "-_underscore" [Ident "-_underscore"]
            testTokenize "-text" [Ident "-text"]
            testTokenize "-\\6d" [Ident "-m"]
            testTokenize "--abc" [Ident "--abc"]
            testTokenize "--" [Ident "--"]
            testTokenize "--11" [Ident "--11"]
            testTokenize "---" [Ident "---"]
            testTokenize "\x2003" [Ident "\x2003"] -- em-space
            testTokenize "\xA0" [Ident "\xA0"]  -- non-breaking space
            testTokenize "\x1234" [Ident "\x1234"]
            testTokenize "\x12345" [Ident "\x12345"]
            testTokenize "\0" [Ident "\xfffd"]
            testTokenize "ab\0c" [Ident $ "ab\xfffd" <> "c"]

        it "Function" $ do
            testTokenize "scale(2)" [Function "scale", Number "2" (NVInteger 2), RightParen]
            testTokenize "foo-bar\\ baz(" [Function "foo-bar baz"]
            testTokenize "fun\\(ction(" [Function "fun(ction"]
            testTokenize "-foo(" [Function "-foo"]
            testTokenize "url(\"foo.gif\"" [Function "url", String "foo.gif"]
            testTokenize "foo(  \'bar.gif\'" [Function "foo", Whitespace, String "bar.gif"]
            -- // To simplify implementation we drop the whitespace in function(url),whitespace,string()
            testTokenize "url(  \'bar.gif\'" [Function "url", String "bar.gif"]

        it "AtKeyword" $ do
            testTokenize "@at-keyword" [AtKeyword "at-keyword"]
            testTokenize "@hello!" [AtKeyword "hello", Delim '!']
            testTokenize "@-text" [AtKeyword "-text"]
            testTokenize "@--abc" [AtKeyword "--abc"]
            testTokenize "@--" [AtKeyword "--"]
            testTokenize "@--11" [AtKeyword "--11"]
            testTokenize "@---" [AtKeyword "---"]
            testTokenize "@\\ " [AtKeyword " "]
            testTokenize "@-\\ " [AtKeyword "- "]
            testTokenize "@@" [Delim '@', Delim '@']
            testTokenize "@2" [Delim '@', Number "2" (NVInteger 2)]
            testTokenize "@-1" [Delim '@', Number "-1" (NVInteger (-1))]


        it "Url" $ do
            testTokenize "url(foo.gif)" [Url "foo.gif"]
            testTokenize "urL(https://example.com/cats.png)" [Url "https://example.com/cats.png"]
            testTokenize "uRl(what-a.crazy^URL~this\\ is!)" [Url "what-a.crazy^URL~this is!"]
            testTokenize "uRL(123#test)" [Url "123#test"]
            testTokenize "Url(escapes\\ \\\"\\'\\)\\()" [Url "escapes \"')("]
            testTokenize "UrL(   whitespace   )" [Url "whitespace"]
            testTokenize "URl( whitespace-eof " [Url "whitespace-eof"]
            testTokenize "URL(eof" [Url "eof"]
            testTokenize "url(not/*a*/comment)" [Url "not/*a*/comment"]
            testTokenize "urL()" [Url ""]
            testTokenize "uRl(white space)," [BadUrl, Comma]
            testTokenize "Url(b(ad)," [BadUrl, Comma]
            testTokenize "uRl(ba'd):" [BadUrl, Colon]
            testTokenize "urL(b\"ad):" [BadUrl, Colon]
            testTokenize "uRl(b\"ad):" [BadUrl, Colon]
            testTokenize "Url(b\\\rad):" [BadUrl, Colon]
            testTokenize "url(b\\\nad):" [BadUrl, Colon]
            testTokenize "url(/*'bad')*/" [BadUrl, Delim '*', Delim '/']
            testTokenize "url(ba'd\\\\))" [BadUrl, RightParen]

        it "String" $ do
            testTokenize "'text'" [String "text"]
            testTokenize "\"text\"" [String "text"]
            testTokenize "'testing, 123!'" [String "testing, 123!"]
            testTokenize "'es\\'ca\\\"pe'" [String "es'ca\"pe"]
            testTokenize "'\"quotes\"'" [String "\"quotes\""]
            testTokenize "\"'quotes'\"" [String "'quotes'"]
            testTokenize "\"mismatch'" [String "mismatch'"]
            testTokenize "'text\5\t\xb'" [String "text\5\t\xb"]
            testTokenize "\"end on eof" [String "end on eof"]
            testTokenize "'esca\\\nped'" [String "escaped"]
            testTokenize "\"esc\\\faped\"" [String "escaped"]
            testTokenize "'new\\\rline'" [String "newline"]
            testTokenize "\"new\\\r\nline\"" [String "newline"]
            testTokenize "'bad\nstring" [BadString, Whitespace, Ident "string"]
            testTokenize "'bad\rstring" [BadString, Whitespace, Ident "string"]
            testTokenize "'bad\r\nstring" [BadString, Whitespace, Ident "string"]
            testTokenize "'bad\fstring" [BadString, Whitespace, Ident "string"]
            testTokenize "'\0'" [String "\xFFFD"]
            testTokenize "'hel\0lo'" [String "hel\xfffdlo"]
            testTokenize "'h\\65l\0lo'" [String "hel\xfffdlo"]
            testTokenize "'ignore backslash at eof\\" [String "ignore backslash at eof"]

        it "Hash" $ do
            testTokenize "#id-selector" [Hash HId "id-selector"]
            testTokenize "#FF7700" [Hash HId "FF7700"]
            testTokenize "#3377FF" [Hash HUnrestricted "3377FF"]
            testTokenize "#\\ " [Hash HId " "]
            testTokenize "# " [Delim '#', Whitespace]
            testTokenize "#\\\n" [Delim '#', Delim '\\', Whitespace]
            testTokenize "#\\\r\n" [Delim '#', Delim '\\', Whitespace]
            testTokenize "#!" [Delim '#', Delim '!']

        it "Number" $ do
            testTokenize "10" [Number "10" (NVInteger 10)]
            testTokenize "12.0" [Number "12.0" (NVNumber 12)]
            testTokenize "+45.6" [Number "+45.6" (NVNumber 45.6)]
            testTokenize "-7" [Number "-7" (NVInteger (-7))]
            testTokenize "010" [Number "010" (NVInteger 10)]
            testTokenize "10e0" [Number "10e0" (NVNumber 10)]
            testTokenize "12e3" [Number "12e3" (NVNumber 12000)]
            testTokenize "3e+1" [Number "3e+1" (NVNumber 30)]
            testTokenize "12E-1" [Number "12E-1" (NVNumber 1.2)]
            testTokenize ".7" [Number ".7" (NVNumber 0.7)]
            testTokenize "-.3" [Number "-.3" (NVNumber (-0.3))]
            testTokenize "+637.54e-2" [Number "+637.54e-2" (NVNumber 6.3754)]
            testTokenize "-12.34E+2" [Number "-12.34E+2" (NVNumber (-1234))]
            testTokenize "+ 5" [Delim '+', Whitespace, Number "5" (NVInteger 5)]
            testTokenize "-+12" [Delim '-', Number "+12" (NVInteger 12)]
            testTokenize "+-21" [Delim '+', Number "-21" (NVInteger (-21))]
            testTokenize "++22" [Delim '+', Number "+22" (NVInteger 22)]
            testTokenize "13." [Number "13" (NVInteger 13), Delim ('.')]
            testTokenize "1.e2" [Number "1" (NVInteger 1), Delim '.', Ident "e2"]
            testTokenize "2e3.5" [Number "2e3" (NVNumber 2e3), Number ".5" (NVNumber 0.5)]
            testTokenize "2e3." [Number "2e3" (NVNumber 2e3), Delim '.']
            testTokenize "1000000000000000000000000" [Number "1000000000000000000000000" (NVInteger 1000000000000000000000000)]
            testTokenize "123456789223456789323456789423456789523456789623456789723456789823456789923456789" [Number "123456789223456789323456789423456789523456789623456789723456789823456789923456789" (NVInteger 123456789223456789323456789423456789523456789623456789723456789823456789923456789)]
            testTokenize "1.797693134862315708145274237317043567980705675258449965989174768031572607800285387605895586327668781715404589535143824642e308" [Number "1.797693134862315708145274237317043567980705675258449965989174768031572607800285387605895586327668781715404589535143824642e308" (NVNumber 1.797693134862315708145274237317043567980705675258449965989174768031572607800285387605895586327668781715404589535143824642e308)]

        it "Dimension" $ do
            testTokenize "10px" [Dimension "10" (NVInteger 10) "px"]
            testTokenize "12.0em" [Dimension "12.0" (NVNumber 12) "em"]
            testTokenize "-12.0em" [Dimension "-12.0" (NVNumber (-12)) "em"]
            testTokenize "+45.6__qem" [Dimension "+45.6" (NVNumber 45.6) "__qem"]
            testTokenize "5e" [Dimension "5" (NVInteger 5) "e"]
            testTokenize "5px-2px" [Dimension "5" (NVInteger 5) "px-2px"]
            testTokenize "5e-" [Dimension "5" (NVInteger 5) "e-"]
            testTokenize "5\\ " [Dimension "5" (NVInteger 5) " "]
            testTokenize "40\\70\\78" [Dimension "40" (NVInteger 40) "px"]
            testTokenize "4e3e2" [Dimension "4e3" (NVNumber 4e3) "e2"]
            testTokenize "0x10px" [Dimension "0" (NVInteger 0) "x10px"]
            testTokenize "4unit " [Dimension "4" (NVInteger 4) "unit", Whitespace]
            testTokenize "5e+" [Dimension "5" (NVInteger 5) "e", Delim '+']
            testTokenize "2e.5" [Dimension "2" (NVInteger 2) "e", Number ".5" (NVNumber 0.5)]
            testTokenize "2e+.5" [Dimension "2" (NVInteger 2) "e", Number "+.5" (NVNumber 0.5)]
            testTokenize "1-2" [Number "1" (NVInteger 1), Number "-2" (NVInteger (-2))]
            testTokenize "1\\65 1" [Dimension "1" (NVInteger 1) "e1"]
            testTokenize "1\\31 em" [Dimension "1" (NVInteger 1) "1em"]
            testTokenize "1e\\31 em" [Dimension "1" (NVInteger 1) "e1em"]

        it "Percentage" $ do
            testTokenize "10%" [Percentage "10" $ NVInteger 10]
            testTokenize "+12.0%" [Percentage "+12.0" $ NVNumber 12.0]
            testTokenize "-48.99%" [Percentage "-48.99" $ NVNumber (-48.99)]
            testTokenize "6e-1%" [Percentage "6e-1" $ NVNumber 6e-1]
            testTokenize "5%%" [Percentage "5" (NVInteger 5), Delim '%']


-- 402	TEST(CSSTokenizerTest, UnicodeRangeToken)
-- 403	{
-- 404	    TEST_TOKENS("u+012345-123456", unicodeRange(0x012345, 0x123456));
-- 405	    TEST_TOKENS("U+1234-2345", unicodeRange(0x1234, 0x2345));
-- 406	    TEST_TOKENS("u+222-111", unicodeRange(0x222, 0x111));
-- 407	    TEST_TOKENS("U+CafE-d00D", unicodeRange(0xcafe, 0xd00d));
-- 408	    TEST_TOKENS("U+2??", unicodeRange(0x200, 0x2ff));
-- 409	    TEST_TOKENS("U+ab12??", unicodeRange(0xab1200, 0xab12ff));
-- 410	    TEST_TOKENS("u+??????", unicodeRange(0x000000, 0xffffff));
-- 411	    TEST_TOKENS("u+??", unicodeRange(0x00, 0xff));
-- 412
-- 413	    TEST_TOKENS("u+222+111", unicodeRange(0x222, 0x222), number(IntegerValueType, 111, PlusSign));
-- 414	    TEST_TOKENS("u+12345678", unicodeRange(0x123456, 0x123456), number(IntegerValueType, 78, NoSign));
-- 415	    TEST_TOKENS("u+123-12345678", unicodeRange(0x123, 0x123456), number(IntegerValueType, 78, NoSign));
-- 416	    TEST_TOKENS("u+cake", unicodeRange(0xca, 0xca), ident("ke"));
-- 417	    TEST_TOKENS("u+1234-gggg", unicodeRange(0x1234, 0x1234), ident("-gggg"));
-- 418	    TEST_TOKENS("U+ab12???", unicodeRange(0xab1200, 0xab12ff), delim('?'));
-- 419	    TEST_TOKENS("u+a1?-123", unicodeRange(0xa10, 0xa1f), number(IntegerValueType, -123, MinusSign));
-- 420	    TEST_TOKENS("u+1??4", unicodeRange(0x100, 0x1ff), number(IntegerValueType, 4, NoSign));
-- 421	    TEST_TOKENS("u+z", ident("u"), delim('+'), ident("z"));
-- 422	    TEST_TOKENS("u+", ident("u"), delim('+'));
-- 423	    TEST_TOKENS("u+-543", ident("u"), delim('+'), number(IntegerValueType, -543, MinusSign));

        it "Comment" $ do
            testTokenize "/*comment*/a" [Ident "a"]
            testTokenize "/**\\2f**//" [Delim '/']
            testTokenize "/**y*a*y**/ " [Whitespace]
            testTokenize ",/* \n :) \n */)" [Comma, RightParen]
            testTokenize ":/*/*/" [Colon]
            testTokenize "/**/*" [Delim '*']
            testTokenize ";/******" [Semicolon]

        it "Serialization" $ do
            testSerialize [String "hello,\x0world"] "\"hello,\xFFFDworld\""
            testSerialize [String "\x10\x7f\""] "\"\\10 \\7f \\\"\""
            testSerialize [String "\xABCDE"] "\"\xABCDE\""
            testSerialize [Ident "-"] "\\-"
            testSerialize [Ident "5"] "\\35 "
            testSerialize [Ident "-5"] "-\\35 "
            testSerialize [Ident "-9f\\oo()"] "-\\39 f\\\\oo\\(\\)"
            testSerialize [Ident "\xABCDE"] "\xABCDE"
            testSerialize [Ident "a", Ident "b"] "a/**/b"
            testSerialize [Dimension "1" (NVInteger 1) "em", Ident "b"] "1em/**/b"

        it "Doesn't regress Tokenize=>serialize=>tokenize roundtrip" $ do
            let a = tokenize "\\-->"
            tokenize (serialize a) `shouldBe` a

        modifyMaxSize (const 500) $ modifyMaxSuccess (const 100000) $
            prop "Tokenize=>serialize=>tokenize roundtrip"
                prop_tstRoundTrip
        modifyMaxSize (const 50) $ modifyMaxSuccess (const 100000) $
            prop "Serialize=>tokenize roundtrip"
                prop_stRoundTrip

prop_tstRoundTrip :: String -> Bool
prop_tstRoundTrip s = tokenize (serialize t) == t
    where t = tokenize $ T.pack s

prop_stRoundTrip :: TestTokens -> Bool
prop_stRoundTrip (TestTokens t) = tokenize (serialize t) == t

newtype TestTokens = TestTokens [Token]
    deriving (Show, Eq)

instance Arbitrary TestTokens where
    arbitrary = TestTokens . go <$> arbitrary
        where go [] = []
              go (x : xs@(Whitespace : _)) | needWhitespace x = x : go xs
              -- we can only have [BadString, Whitespace]
              -- or [Delim '\\', Whitespace] sequences
              go (x : xs) | needWhitespace x = x : Whitespace : go xs
              go (x@(Function (T.toLower -> "url")) : xs) =
                  x : String "argument" : go xs
              go (x : xs) = x : go xs
              needWhitespace x = case x of
                  BadString -> True
                  Delim '\\' -> True
                  _ -> False


instance Arbitrary Token where
    arbitrary = oneof $
        map return
        [ Whitespace
        , CDO
        , CDC

        , Comma
        , Colon
        , Semicolon

        , LeftParen
        , RightParen
        , LeftSquareBracket
        , RightSquareBracket
        , LeftCurlyBracket
        , RightCurlyBracket

        , SuffixMatch
        , SubstringMatch
        , PrefixMatch
        , DashMatch
        , IncludeMatch

        , Column

        , BadString
        , BadUrl
        ]
        ++
        [ String <$> text
        , num Number
        , num Percentage
        , num Dimension <*> ident

        , Url <$> text

        , Ident <$> ident

        , AtKeyword <$> ident

        , Function <$> ident
        , return $ Function "url"

        , Hash HId <$> ident

        , Delim <$> elements possibleDelimiters
        ]
        where ident = notEmpty text
              notEmpty x = do
                  r <- x
                  if r /= "" then return r else notEmpty x
              text = T.replace "\NUL" "\xFFFD" . T.pack <$> arbitrary
              num token = do
                  c <- arbitrary
                  e <- arbitrary
                  let (t, n)
                          | e /= 0 = nv NVNumber (scientific c e)
                          | otherwise = nv NVInteger c
                      nv f x = (T.pack $ show x, f x)
                  return $ token t n
              possibleDelimiters =
                  [d | c <- ['\0'..'\xff']
                  , [Delim d] <- [tokenize (T.pack [c])]]
                  ++ ['\\']

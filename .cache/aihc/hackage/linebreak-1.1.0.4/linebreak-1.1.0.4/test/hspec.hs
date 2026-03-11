import Test.Hspec
import Text.LineBreak


hyp :: Maybe Hyphenator
hyp = Just english_US

bf :: BreakFormat
bf = BreakFormat 25 4 '-' hyp

testBr :: String -> String
testBr s = breakString bf s

myUnlines :: [String]-> [Char]
myUnlines = init . unlines -- unlines adds an extra "\n" ad end-of-string


main :: IO ()
main = hspec $ do

  describe "Text.Linebreak.breakString" $ do

    let sip    = "Using hyphenation with gruesomely non parsimonious wording."
        sipout = myUnlines ["Using hyphenation with",
                            "gruesomely non parsimo-",
                            "nious wording." ]
    it "breaks a string to certain width" $
      testBr sip `shouldBe` sipout

    let sipoutNH = myUnlines ["Using hyphenation with",
                              "gruesomely non",
                              "parsimonious wording." ]
        bfnh = BreakFormat 25 4 '-' Nothing
    it "breaks a string to certain width (w/o hypenator)" $
      breakString bfnh sip `shouldBe` sipoutNH

    let postws    = "word      "
        postwsout = "word"
    it "ws at end of word should be truncated" $
      testBr postws `shouldBe` postwsout

    let postwscc    = "aaa" ++ replicate (25 - 3 - 1) ' ' ++ "bbb"
        postwsccout = "aaa\nbbb"
    it "no end-of-line white-space, corner case (words added after it)" $
      testBr postwscc `shouldBe` postwsccout

    let broken    = "broken\nstring"
        brokenout = myUnlines ["broken", "string"]
    it "should not remove \\n already in the string" $
      testBr broken `shouldBe` brokenout

    let brokenL    = "broken\n\n\n\nstring"
        brokenoutL = "broken\n\n\n\nstring"
    it "should respect multiple '\\n' already in the string" $
      testBr brokenL `shouldBe` brokenoutL

    let spaced    = "spaced   string"
        spacedout = spaced
    it "should not remove blankspace already in the string" $
      testBr spaced `shouldBe` spacedout

    let simple    = "simple"
        simpleout = simple
    it "should not add extra \\n at the end of the string" $
      testBr simple `shouldBe` simpleout

    let prews    = "     word"
        prewsout = prews
    it "should not remove whitespace @ beginning of string" $
      testBr prews `shouldBe` prewsout

    let muchws    =          "word                              second"
        muchwsout = myUnlines ["word", "second"]
    it "should truncate eol whitespace" $
      testBr muchws `shouldBe` muchwsout

    let toolong    = replicate 24 'a' ++ "bbb"
        toolongout = replicate 24 'a' ++ "b\nbb"
    it "should truncate words longer than screenwidth" $
      testBr toolong `shouldBe` toolongout

    -- TODO what happens with really tiny width?
    let tinywd    = "tomcat"
        tinywdout = "to\nmc\nat"
        tinybf = BreakFormat 2 4 '-' (Just english_US)
        tinyBr = breakString tinybf
    it "should wrap (but not hyphenate long words)" $
      tinyBr tinywd `shouldBe` tinywdout

    let bf10 = bf { bfMaxCol = 8 }
    xit "should respect non-breaking space" $
      breakStringLn bf10 "ada la\x00a0mamma" `shouldBe` ["ada", "la mamma"]

  describe "Text.Linebreak.breakStringLn" $ do

    let sip = "Using hyphenation with gruesomely non parsimonious wording."
    it "is the same as |lines $ breakString bf cs|" $ do
      breakStringLn bf sip `shouldBe` lines (breakString bf sip)


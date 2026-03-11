{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString (ByteString)
import Data.Csv hiding (decode, decodeWith, decodeByName, decodeByNameWith)
import Data.Csv.Parser.Megaparsec
import Data.Vector (Vector)
import Test.Hspec
import Test.Hspec.Megaparsec
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector          as V

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "decode"           decodeSpec
  describe "decodeWith"       decodeWithSpec
  describe "decodeByName"     decodeByNameSpec
  describe "decodeByNameWith" decodeByNameWithSpec

decodeSpec :: Spec
decodeSpec = do
  let dec  = decode NoHeader ""
  it "decodes simple data" $
    dec "a,b,c\n" `shouldParse`
      φ [["a", "b", "c"]]
  it "decodes simple data skipping header" $
    decode HasHeader "" "field1,field2,field3\na,b,c\n" `shouldParse`
      φ [["a", "b", "c"]]
  it "handles CRLF sequence correctly" $
    dec "a,b\r\nc,d\r\n" `shouldParse`
      φ [["a", "b"], ["c", "d"]]
  it "handles missing end of line" $
    dec "a,b,c" `shouldParse`
      φ [["a", "b", "c"]]
  it "parses the empty line all right" $
    dec "a,b,c\n\nd,e,f\n\n" `shouldParse`
      φ [["a", "b", "c"], [""], ["d", "e", "f"], [""]]
  it "handles leading space" $
    dec " a,  b,   c\n" `shouldParse`
      φ [[" a", "  b", "   c"]]
  it "parses the RFC 4180 test data correctly" $
    dec rfc4180Input `shouldParse`
      φ rfc4180Output

decodeWithSpec :: Spec
decodeWithSpec =
  context "using tab as delimiter" $ do
    let dec = decodeWith defaultDecodeOptions { decDelimiter = 9 } NoHeader ""
    it "works fine with tab as delimiter" $
      dec "a\tb\tc\n" `shouldParse`
        φ [["a", "b", "c"]]
    it "handles CRLF sequence correctly" $
      dec "a\tb\r\nc\td\r\n" `shouldParse`
        φ [["a", "b"], ["c", "d"]]
    it "handles missing end of line" $
      dec "a\tb\tc" `shouldParse`
        φ [["a", "b", "c"]]
    it "parses the empty line all right" $
      dec "a\tb\tc\n\nd\te\tf\n\n" `shouldParse`
        φ [["a", "b", "c"], [""], ["d", "e", "f"], [""]]
    it "handles leading space" $
      dec " a\t  b\t   c\n" `shouldParse`
        φ [[" a", "  b", "   c"]]

decodeByNameSpec :: Spec
decodeByNameSpec = do
  let dec = decodeByName ""
  it "decodes simple data" $
    dec "field\r\nabc\r\n" `shouldParse`
      χ ["field"] [[("field", "abc")]]
  it "decodes entries with two fields" $
    dec "field1,field2\r\nabc,def\r\n" `shouldParse`
      χ ["field1", "field2"] [[("field1", "abc"), ("field2", "def")]]
  it "decodes header ending with CRLF" $
    dec "field\r\nabc" `shouldParse`
      χ ["field"] [[("field", "abc")]]
  it "decodes data with training CRLF" $
    dec "field\r\nabc\r\n" `shouldParse`
      χ ["field"] [[("field", "abc")]]
  it "decodes multiply entries (CRLF separated)" $
    dec "field\r\nabc\r\ndef" `shouldParse`
      χ ["field"] [[("field", "abc")],[("field","def")]]

decodeByNameWithSpec :: Spec
decodeByNameWithSpec =
  context "using tab as delimiter" $ do
    let dec = decodeByNameWith defaultDecodeOptions { decDelimiter = 9 } ""
    it "decodes simple data" $
      dec "field\r\nabc\r\n" `shouldParse`
        χ ["field"] [[("field", "abc")]]
    it "decodes entries with two fields" $
      dec "field1\tfield2\r\nabc\tdef\r\n" `shouldParse`
        χ ["field1", "field2"] [[("field1", "abc"), ("field2", "def")]]
    it "decodes header ending with CRLF" $
      dec "field\r\nabc" `shouldParse`
        χ ["field"] [[("field", "abc")]]
    it "decodes data with training CRLF" $
      dec "field\r\nabc\r\n" `shouldParse`
        χ ["field"] [[("field", "abc")]]
    it "decodes multiply entries (CRLF separated)" $
      dec "field\r\nabc\r\ndef" `shouldParse`
        χ ["field"] [[("field", "abc")],[("field","def")]]

----------------------------------------------------------------------------
-- Helpers

φ :: [[ByteString]] -> Vector [ByteString]
φ = V.fromList

χ :: [ByteString] -> [[(ByteString, ByteString)]] -> (Header, Vector NamedRecord)
χ h xs = (header h, V.fromList (namedRecord <$> xs))

rfc4180Input :: BL.ByteString
rfc4180Input = BL.concat
  [ "#field1,field2,field3\n"
  , "\"aaa\",\"bb\n"
  , "b\",\"ccc\"\n"
  , "\"a,a\",\"b\"\"bb\",\"ccc\"\n"
  , "zzz,yyy,xxx\n" ]

rfc4180Output :: [[ByteString]]
rfc4180Output =
  [ ["#field1", "field2", "field3"]
  , ["aaa", "bb\nb", "ccc"]
  , ["a,a", "b\"bb", "ccc"]
  , ["zzz", "yyy", "xxx"] ]

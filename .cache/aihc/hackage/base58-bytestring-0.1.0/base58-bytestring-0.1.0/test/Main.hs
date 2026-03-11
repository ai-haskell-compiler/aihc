module Main where

import Data.ByteString ( ByteString )
import Data.ByteString.Base58
import Data.Monoid
import Test.QuickCheck.Assertions
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck

prop_EncodeDecode :: Alphabet -> ByteString -> Result
prop_EncodeDecode alpha bs =
    (decodeBase58 alpha $ encodeBase58 alpha bs) ?== (Just bs)

prop_EncodeDecodeInt :: Alphabet -> (NonNegative Integer) -> Result
prop_EncodeDecodeInt alpha (NonNegative i) =
    (decodeBase58I alpha $ encodeBase58I alpha i) ?== (Just i)


main :: IO ()
main = do
    defaultMain
        $ testGroup "base58 transcoding"
        $ [ ("bitcoinAlphabet", bitcoinAlphabet)
          , ("rippleAlphabet", rippleAlphabet)
          , ("flickrAlphabet", flickrAlphabet)
          ] >>= \(aname, alpha) ->
                    [ testProperty (aname <> ": decode . encode ~ id")
                      $ prop_EncodeDecode alpha
                    , testProperty (aname <> ": encode . decode ~ id")
                      $ prop_EncodeDecodeInt alpha
                    ]

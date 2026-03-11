{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Test.QuickCheck       ((===))
import Test.Tasty            (defaultMain, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (..), label, testProperty)

import qualified Data.ByteString as BS
import qualified Data.Text       as T

import Data.Integer.Conversion

import qualified Alternative
import qualified Naive

main :: IO ()
main = defaultMain $ testGroup "integer-conversion"
    [ testGroup "text"
        [ testProperty "naive" $ \t -> labelT t $ textToInteger t === Naive.textToInteger t
        , testProperty "alt"   $ \t -> labelT t $ textToInteger t === Alternative.textToInteger t
        ]
    , testGroup "bs"
        [ testProperty "naive" $ \bs -> labelB bs $ byteStringToInteger bs === Naive.byteStringToInteger bs
        , testProperty "alt"   $ \bs -> labelB bs $ byteStringToInteger bs === Alternative.byteStringToInteger bs
        ]
    , testGroup "string"
        [ testProperty "naive" $ \s -> labelS s $ stringToInteger s === Naive.stringToInteger s
        , testProperty "alt"   $ \s -> labelS s $ stringToInteger s === Alternative.stringToInteger s
        ]
    ]
  where
    labelT t = label (if T.length t  >= 40 then "long" else "short")
    labelB b = label (if BS.length b >= 40 then "long" else "short")
    labelS s = label (if length s    >= 40 then "long" else "short")

-------------------------------------------------------------------------------
-- Orphans
-------------------------------------------------------------------------------

-- we could use quickcheck-instances,
-- but by defining these instances here we make adopting newer GHC smoother.

instance Arbitrary T.Text where
    arbitrary = fmap T.pack arbitrary

instance Arbitrary BS.ByteString where
    arbitrary = fmap BS.pack arbitrary

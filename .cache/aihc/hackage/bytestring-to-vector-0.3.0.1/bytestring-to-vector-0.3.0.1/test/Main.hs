{-# LANGUAGE
    ScopedTypeVariables #-}

module Main
    ( main
    )
where

import qualified Data.ByteString               as B
import qualified Data.Vector.Storable          as V

import           Data.Word
import           System.ByteOrder   -- package 'byteorder'
import           Test.QuickCheck
import           Foreign.Storable

import           Data.Vector.Storable.ByteString


arbList :: (Bounded a, Integral a) => Gen [a]
arbList = choose (0, 16384) >>= flip vectorOf arbitraryBoundedIntegral

instance Arbitrary B.ByteString where
    arbitrary = fmap B.pack arbList

instance (Bounded a, Integral a, Storable a) => Arbitrary (V.Vector a) where
    arbitrary = fmap V.fromList arbList


-- Reference implementations

fromBE, fromHost :: (Integral a) => [Word8] -> a
fromBE = foldl (\n x -> n * 256 + fromIntegral x) 0
fromHost = case byteOrder of
    LittleEndian -> fromBE . reverse
    BigEndian    -> fromBE

toLE, toHost :: (Integral a) => Int -> a -> [Word8]
toLE sz = take sz . map (fromIntegral . (`mod` 256)) . iterate (`div` 256)
toHost = case byteOrder of
    LittleEndian -> toLE
    BigEndian    -> \sz -> reverse . toLE sz

data Rep a = Rep Int ([Word8] -> a) (a -> [Word8])

rep8 :: Rep Word8
rep8 = Rep 1 (\[x] -> x) return

repN :: forall a . (Storable a, Integral a) => Rep a
repN = let n = sizeOf (undefined :: a) in Rep n fromHost (toHost n)

ref_byteStringToVector :: (Storable a) => Rep a -> B.ByteString -> V.Vector a
ref_byteStringToVector (Rep n f _) = V.fromList . go  where
    go bs = case B.splitAt n bs of
        (x, xs) | B.length x == n -> f (B.unpack x) : go xs
        _                         -> []

ref_vectorToByteString :: (Storable a) => Rep a -> V.Vector a -> B.ByteString
ref_vectorToByteString (Rep _ _ f) = B.pack . concatMap f . V.toList


-- Properties

mkProp_inv _ x = byteStringToVector (vectorToByteString x) == x
mkProp_inv_ref r x = ref_byteStringToVector r (ref_vectorToByteString r x) == x

mkProp_inv_ref1 r x = ref_byteStringToVector r (vectorToByteString x) == x
mkProp_inv_ref2 r x = byteStringToVector (ref_vectorToByteString r x) == x

mkProp_eq_BV r x = ref_byteStringToVector r x == byteStringToVector x
mkProp_eq_VB r x = ref_vectorToByteString r x == vectorToByteString x


-- Test runner

runFor :: (Integral a, Bounded a, Storable a, Show a) => Rep a -> IO ()
runFor r = do
    mapM_
        (quickCheck . ($ r))
        [ mkProp_inv
        , mkProp_inv_ref
        , mkProp_inv_ref1
        , mkProp_inv_ref2
        , mkProp_eq_VB
        ]
    quickCheck (mkProp_eq_BV r)

main :: IO ()
main = do
    runFor rep8
    runFor (repN :: Rep Word16)
    runFor (repN :: Rep Word32)
    runFor (repN :: Rep Word64)

module System.Linux.Btrfs.UUID
    ( UUID(..)
    , toString
    , fromString
    ) where

import Data.Word (Word64)
import Data.Word.Endian (BE64(..))
import Data.Bits ((.&.), unsafeShiftR)
import Text.Printf (printf)

import Foreign.Storable (Storable(..))
import Foreign.Ptr (castPtr)
import Foreign.C.Types (CInt)

data UUID = UUID Word64 Word64
  deriving (Eq, Ord)

instance Show UUID where
    showsPrec p u =
        showParen (p > 9) $
            showString "fromString " . shows (toString u)

-- | A @UUID@ is stored as two big-endian 'Word64's.
instance Storable UUID where
    sizeOf _ = 16
    alignment _ = alignment (undefined :: CInt)
    peek ptr = do
        BE64 h  <- peek (castPtr ptr)
        BE64 l  <- peekByteOff ptr 8
        return $ UUID h l
    poke ptr (UUID h l) = do
        poke (castPtr ptr) (BE64 h)
        pokeByteOff ptr 8 (BE64 l)

toString :: UUID -> String
toString (UUID h l) = printf "%.8x-%.4x-%.4x-%.4x-%.12x" h1 h2 h3 l1 l2
  where
    h1 = (h .&. 0xffffffff00000000) `unsafeShiftR` 32
    h2 = (h .&.         0xffff0000) `unsafeShiftR` 16
    h3 = (h .&.             0xffff)
    l1 = (l .&. 0xffff000000000000) `unsafeShiftR` 48
    l2 = (l .&.     0xffffffffffff)

fromString :: String -> Maybe UUID
fromString s
    | isValidUUID s = Just $ UUID h l
    | otherwise = Nothing
  where
    h = read $ '0' : 'x' : take 16 s'
    l = read $ '0' : 'x' : drop 16 s'
    s' = filter (/= '-') s

isValidUUID :: String -> Bool
isValidUUID s = length s == 36 && and (zipWith checkChar [0..] s)
  where
    checkChar i c =
        if i `elem` hyphenPosns then
            c == '-'
        else
            ('0' <= c && c <= '9') || ('a' <= c && c <= 'f')|| ('A' <= c && c <= 'F')
    hyphenPosns = [8, 13, 18, 23] :: [Int]

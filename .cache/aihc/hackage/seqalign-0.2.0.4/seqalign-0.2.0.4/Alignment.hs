{-# LANGUAGE OverloadedStrings #-}
module Alignment where

import Foreign.Ptr
import Foreign.ForeignPtr.Safe
import Foreign.ForeignPtr.Unsafe
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.C.Types
import Control.Applicative
import System.IO.Unsafe
import Data.Word
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Unsafe
import qualified Data.Vector.Storable as V
import Data.Bits

foreign import ccall unsafe "c_degenshtein" cDist :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> IO CInt

editDistance :: B.ByteString -> B.ByteString -> Int
editDistance b1@(B.PS ps1 s1 l1) b2@(B.PS ps2 s2 l2) = unsafePerformIO $ do 
    let p1 = unsafeForeignPtrToPtr ps1
    let p2 = unsafeForeignPtrToPtr ps2
    r <- cDist (castPtr p1 `plusPtr` s1) (fromIntegral l1) (castPtr p2 `plusPtr` s2) (fromIntegral l2)
    touchForeignPtr ps1
    touchForeignPtr ps2
    return (fromIntegral r)

data KSWR = KSWR {
    score :: CInt,
    targend :: CInt,
    querend :: CInt,
    score2 :: CInt,
    targend2 :: CInt,
    targbeg :: CInt,
    querbeg :: CInt } deriving (Show, Read, Eq)

instance Storable KSWR where
    sizeOf _ = 28
    alignment = sizeOf
    peek ptr = KSWR <$> pb 0 <*> pb 4 <*> pb 8 <*> pb 12 <*> pb 16 <*> pb 20 <*> pb 24 where 
        pb = peekByteOff ptr

foreign import ccall "default_align" cLocalAlign ::
    CInt -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> CInt -> CInt -> Ptr KSWR -> IO ()

bs_align :: B.ByteString -> B.ByteString -> KSWR
bs_align (B.PS ptra offa lena) (B.PS ptrb offb lenb) = unsafePerformIO $ do
    let aptr = unsafeForeignPtrToPtr ptra `plusPtr` offa
        bptr = unsafeForeignPtrToPtr ptrb `plusPtr` offb
    kswrptr <- mallocBytes 28
    cLocalAlign (fromIntegral lena) aptr (fromIntegral lenb) bptr 5 2 (0x80000 + 0x40000) kswrptr
    kswr <- peek kswrptr
    touchForeignPtr ptra
    touchForeignPtr ptrb
    free kswrptr
    return kswr

foreign import ccall "cigar_align" cCigarAlign ::
    CInt -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> CInt -> CInt -> Ptr CInt -> Ptr (Ptr CInt) -> IO CInt

foreign import ccall unsafe "static stdlib.h &free" free_finalizer
    :: FunPtr (Ptr a -> IO ())

cigar_align :: B.ByteString -> B.ByteString -> (Int, [(Char,Int)])
cigar_align (B.PS ptra offa lena) (B.PS ptrb offb lenb) = unsafePerformIO $ do
    let aptr = unsafeForeignPtrToPtr ptra `plusPtr` offa
        bptr = unsafeForeignPtrToPtr ptrb `plusPtr` offb
    ncigar_p <- mallocBytes 4
    cigar_pp <- mallocBytes 8
    r <- cCigarAlign (fromIntegral lena) aptr (fromIntegral lenb) bptr 1 1 10 ncigar_p cigar_pp
    ncigar <- peek ncigar_p
    cigar_p <- peek cigar_pp
    touchForeignPtr ptra
    touchForeignPtr ptrb
    free ncigar_p
    free cigar_pp
    fp <- newForeignPtr free_finalizer cigar_p
    let v = V.unsafeFromForeignPtr0 fp (fromIntegral ncigar)
        cigar = map pcigar $ V.toList v
        pcigar i = (code, fromIntegral $ i `shiftR` 4) where
            code = case i .&. 15 of
                0 -> 'M'
                1 -> 'I'
                2 -> 'D'
                3 -> 'N'
                4 -> 'S'
                5 -> 'H'
                6 -> 'P'
                7 -> '='
                8 -> 'X'
    return (fromIntegral r, cigar)

{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BlockArguments #-}
-- {-# LANGUAGE DataKinds #-} -- needed for manual ZeroBitType def (unsure why)
-- {-# LANGUAGE FlexibleInstances #-}

{- | Struct parser.

We do still have to do failure checking, because unlike C we check some types
(e.g. bitfields). Hopefully inlining can remove those checks when unnecessary.
-}

module Bytezap.Parser.Struct where

import GHC.Exts
import GHC.ForeignPtr
import Data.Void ( Void )

import Data.Word ( Word8 )
import Data.ByteString.Internal qualified as B
import System.IO.Unsafe ( unsafePerformIO )

import Raehik.Compat.Data.Primitive.Types

import Data.Bits
  ( Bits( (.&.), unsafeShiftR, xor )
  , FiniteBits(countTrailingZeros)
  )

type PureMode = Proxy# Void
type IOMode   = State# RealWorld
type STMode s = State# s

type ParserT# (st :: ZeroBitType) e a =
       ForeignPtrContents {- ^ pointer provenance (does not change) -}
    -> Addr#              {- ^ base address (does not change) -}
    -> Int#               {- ^ cursor offset from base -}
    -> st                 {- ^ state token -}
    -> Res# st e a        {- ^ result -}

{- | Like flatparse, but no buffer length (= no buffer overflow checking), and
     no 'Addr#' on success (= no dynamic length parses).

Unlike flatparse, we separate base address from offset, rather than adding
them. This fits the unaligned 'Addr#' primops (added in GHC 9.10) better, and
in my head should hopefully assist in emitting immediates where possible for
offsets on the assembly level.

Combining them like in flatparse might be faster; but I really don't know how to
find out, without doing both and comparing various examples. After a lot of
scratching my head, I think this is most appropriate.

The 'ForeignPtrContents' is for keeping the 'Addr#' data in scope.
-}
newtype ParserT (st :: ZeroBitType) e a =
    ParserT { runParserT# :: ParserT# st e a }

instance Functor (ParserT st e) where
  fmap f (ParserT g) = ParserT \fpc base os st0 -> case g fpc base os st0 of
    OK# st1 a -> let !b = f a in OK# st1 b
    x         -> unsafeCoerce# x
  {-# inline fmap #-}

-- No Applicative due to no offset passing.

-- | The type of pure parsers.
type Parser     = ParserT PureMode

-- | The type of parsers which can embed `IO` actions.
type ParserIO   = ParserT IOMode

-- | The type of parsers which can embed `ST` actions.
type ParserST s = ParserT (STMode s)

-- | Primitive parser result wrapped with a state token.
--
-- You should rarely need to manipulate values of this type directly. Use the
-- provided bidirectional pattern synonyms 'OK#', 'Fail#' and 'Err#'.
type Res# (st :: ZeroBitType) e a =
  (# st, ResI# e a #)

-- | Primitive parser result.
--
-- Like flatparse, but no 'Addr#' on success.
type ResI# e a =
  (#
    (# a #)
  | (# #)
  | (# e #)
  #)

-- | 'Res#' constructor for a successful parse.
--   Contains the return value and a state token.
pattern OK# :: (st :: ZeroBitType) -> a -> Res# st e a
pattern OK# st a = (# st, (# (# a #) | | #) #)

-- | 'Res#' constructor for recoverable failure.
--   Contains only a state token.
pattern Fail# :: (st :: ZeroBitType) -> Res# st e a
pattern Fail# st = (# st, (# | (# #) | #) #)

-- | 'Res#' constructor for errors which are by default non-recoverable.
--    Contains the error, plus a state token.
pattern Err# :: (st :: ZeroBitType) -> e -> Res# st e a
pattern Err# st e = (# st, (# | | (# e #) #) #)
{-# complete OK#, Fail#, Err# #-}

-- | caller must guarantee that buffer is long enough for parser!!
unsafeRunParserBs :: forall a e. B.ByteString -> Parser e a -> Result e a
unsafeRunParserBs (B.BS fptr _) = unsafeRunParserFPtr fptr

-- | caller must guarantee that buffer is long enough for parser!!
unsafeRunParserPtr :: forall a e. Ptr Word8 -> Parser e a -> Result e a
unsafeRunParserPtr (Ptr base#) = unsafeRunParser' base# FinalPtr

-- | caller must guarantee that buffer is long enough for parser!!
unsafeRunParserFPtr :: forall a e. ForeignPtr Word8 -> Parser e a -> Result e a
unsafeRunParserFPtr fptr p =
    unsafePerformIO $ B.unsafeWithForeignPtr fptr $ \ptr ->
        pure $ unsafeRunParserPtr ptr p

-- | caller must guarantee that buffer is long enough for parser!!
unsafeRunParser'
    :: forall a e. Addr# -> ForeignPtrContents -> Parser e a -> Result e a
unsafeRunParser' base# fpc (ParserT p) =
    case p fpc base# 0# proxy# of
      OK#   _st1 a -> OK a
      Err#  _st1 e -> Err e
      Fail# _st1   -> Fail

-- | Higher-level boxed data type for parsing results.
data Result e a =
    OK a    -- ^ Contains return value.
  | Fail    -- ^ Recoverable-by-default failure.
  | Err !e  -- ^ Unrecoverable-by-default error.
  deriving Show

-- | can't provide via 'pure' as no 'Applicative'
constParse :: a -> ParserT st e a
constParse a = ParserT \_fpc _base _os st -> OK# st a

sequenceParsers
    :: Int -> (a -> b -> c)
    -> ParserT st e a -> ParserT st e b -> ParserT st e c
sequenceParsers (I# len#) f (ParserT pa) (ParserT pb) =
    ParserT \fpc base os# st0 ->
        case pa fpc base os# st0 of
          OK# st1 a ->
            case pb fpc base (os# +# len#) st1 of
              OK# st2 b -> OK# st2 (f a b)
              Fail# st2 ->  Fail# st2
              Err# st2 e -> Err# st2 e
          Err# st1 e -> Err# st1 e
          Fail# st1 ->  Fail# st1

-- TODO using indexWord8OffAddrAs to permit pure mode. flatparse does this (at
-- least for integers). guess it's OK?
-- TODO this doesn't use the state token. scary.
prim :: forall a st e. Prim' a => ParserT st e a
prim = ParserT \_fpc base os st ->
    case indexWord8OffAddrAs# base os of a -> OK# st a

-- | parse literal
lit :: Eq a => a -> ParserT st e a -> ParserT st e ()
lit al (ParserT p) = ParserT \fpc base os st0 ->
    case p fpc base os st0 of
      OK#   st1 ar -> if al == ar then OK# st1 () else Fail# st1
      Err#  st1 e  -> Err#  st1 e
      Fail# st1    -> Fail# st1

-- | parse literal (CPS)
withLit
    :: Eq a => Int# -> a -> ParserT st e a -> ParserT st e r -> ParserT st e r
withLit len# al (ParserT p) (ParserT pCont) = ParserT \fpc base os# st0 ->
    case p fpc base os# st0 of
      OK#   st1 ar ->
        if al == ar then pCont fpc base (os# +# len#) st1 else Fail# st1
      Err#  st1 e  -> Err#  st1 e
      Fail# st1    -> Fail# st1

{- | parse literal, return first (leftmost) failing byte on error (CPS)

This can be used to parse large literals via chunking, rather than byte-by-byte,
while retaining useful error behaviour.

We don't check equality with XOR even though we use that when handling errors,
because it's hard to tell if it would be faster with modern CPUs and compilers.
-}
withLitErr
    :: (Integral a, FiniteBits a)
    => Int# -> a -> Int -> (Addr# -> Int# -> a)
    -> ParserT st (Int, Word8) r
    -> ParserT st (Int, Word8) r
withLitErr len# aLit idxStart p (ParserT pCont) = ParserT \fpc base# os# st ->
    let aParsed = p base# os#
    in  if   aLit == aParsed
        then pCont fpc base# (os# +# len#) st
        else let idxFail = firstNonMatchByteIdx aLit aParsed
                 bFailed = unsafeByteAt aParsed idxFail
             in  Err# st (idxStart + idxFail, fromIntegral bFailed)
{-# INLINE withLitErr #-}

-- | Given two non-equal words @wActual@ and @wExpect@, return the index of the
--   first non-matching byte. Zero indexed.
--
-- If both words are equal, returns word_size (e.g. 4 for 'Word32').
firstNonMatchByteIdx :: FiniteBits a => a -> a -> Int
firstNonMatchByteIdx wExpect wActual =
    countTrailingZeros (wExpect `xor` wActual) `unsafeShiftR` 3
{-# INLINE firstNonMatchByteIdx #-}

-- | Get the byte at the given index.
--
-- The return value is guaranteed to be 0x00 - 0xFF (inclusive).
--
-- TODO meaning based on endianness?
unsafeByteAt :: (Num a, Bits a) => a -> Int -> a
unsafeByteAt a idx = (a `unsafeShiftR` (idx * 8)) .&. 0xFF
{-# INLINE unsafeByteAt #-}

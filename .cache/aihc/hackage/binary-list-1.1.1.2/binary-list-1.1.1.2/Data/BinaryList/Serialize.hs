
-- | Serialization methods for binary lists.
module Data.BinaryList.Serialize (
     -- * Simple interface
     encode
   , decode
     -- * Other methods
   , Direction (..)
     -- ** Encoding
   , EncodedBinList (..)
   , encodeBinList
     -- ** Decoding
   , DecodedBinList (..)
   , Decoded (..)
   , fromDecoded
   , toDecoded
   , decodedToList
   , decodeBinList
     -- ** ByteString translations
   , encodedToByteString
   , encodedFromByteString
   ) where

import Data.Foldable (traverse_)
-- Binary lists
import Data.BinaryList.Internal
import Data.BinaryList
-- Binary package
import Data.Binary (Binary (..))
import Data.Binary.Put
import Data.Binary.Get
-- Bytestrings
import Data.ByteString.Lazy (ByteString,empty)
-- Backwards Applicative
import Control.Applicative.Backwards
-- DeepSeq
import Control.DeepSeq (NFData (..))

-- | Encode a binary list using the 'Binary' instance of
--   its elements.
encode :: Binary a => BinList a -> ByteString
encode = encodedToByteString . encodeBinList put FromLeft

-- | Decode a binary list using the 'Binary' instance of
--   its elements. It returns a 'String' in case of
--   decoding failure.
decode :: Binary a => ByteString -> Either String (BinList a)
decode input = encodedFromByteString input >>= fromDecoded . decData . decodeBinList get

-- | Direction of encoding. If the direction is 'FromLeft',
--   the binary list will be encoded from left to right. If
--   the direction is 'FromRight', the binary list will be
--   encoded in the opposite way. Choose a direction according
--   to the part of the list you want to have access earlier.
--   If you foresee reading only a part of the list, either
--   at its beginning or end, an appropiate choice of direction
--   will allow you to avoid decoding the full list.
data Direction = FromLeft | FromRight deriving (Eq,Show)

-- | A binary list encoded, ready to be written in a file or be
--   sent over a network. It can be directly translated to a
--   'ByteString' using 'encodedToByteString', or restored
--   using 'encodedFromByteString'.
data EncodedBinList =
  EncodedBinList
    { -- | Direction of encoding.
      encDirection :: Direction
      -- | Length exponent (see 'lengthExponent') of the binary list.
    , encLength :: Exponent
      -- | Encoded data.
    , encData :: ByteString
      }

-- | Encode a binary list, using a custom serialization for its elements and
--   an user-supplied direction.
encodeBinList :: (a -> Put) -> Direction -> BinList a -> EncodedBinList
encodeBinList f d xs = EncodedBinList d (lengthExponent xs) $
  if d == FromLeft
     then runPut $ traverse_ f xs
     else runPut $ forwards $ traverse_ (Backwards . f) xs

-- | A binary list decoded, from where you can extract a binary list. If the
--   decoding process fails in some point, you still will be able to retrieve
--   the binary list of elements that were decoded successfully before the
--   error.
data DecodedBinList a =
  DecodedBinList
    { -- | Direction of encoding.
      decDirection :: Direction
      -- | Length exponent (see 'lengthExponent') of the binary list.
    , decLength :: Exponent
      -- | Decoded data.
    , decData :: Decoded a
      }

-- | The result of decoding a binary list, which produces a list of binary
--   lists of increasing size, ending in either a decoding error or a final
--   binary list. When this is the result of 'decodeBinList', it
--   contains sublists of order 1, 2, 4, 8, ... up to the order of the total
--   list (unless an error has been encountered first). These sublists are
--   either a section starting at the left, or a section starting at the right,
--   depending on the 'Direction' of encoding.
data Decoded a = -- | Partial binary list, and rest of decoded input.
                 PartialResult (BinList a) (Decoded a)
                 -- | Full binary list and remaining input.
               | FinalResult (BinList a) ByteString
                 -- | A decoding error, with an error message and the remaining input.
               | DecodingError String ByteString
                 deriving Show

instance NFData a => NFData (Decoded a) where
  rnf (PartialResult xs  d) = rnf xs  `seq` rnf d
  rnf (FinalResult   xs  b) = rnf xs  `seq` rnf b
  rnf (DecodingError str b) = rnf str `seq` rnf b

instance Functor Decoded where
  fmap f (PartialResult xs  d) = PartialResult (fmap f xs) $ fmap f d
  fmap f (FinalResult   xs  b) = FinalResult   (fmap f xs) b
  fmap _ (DecodingError str b) = DecodingError str b

-- | Get the final result of a decoding process, unless it returned an error, in which
--   case this error is returned as a 'String'.
fromDecoded :: Decoded a -> Either String (BinList a)
fromDecoded (PartialResult _ d) = fromDecoded d
fromDecoded (FinalResult xs _) = Right xs
fromDecoded (DecodingError err _) = Left err

-- | Break a list down to sublists of order 1, 2, 4, 8, ..., 2^k.
--   The result is stored in a 'Decoded' value. Obviously, the output
--   will not have a decoding error.
toDecoded :: BinList a -> Decoded a
toDecoded xs =
  case split xs of
    Right (l,_) -> go l $ FinalResult xs empty
    _ -> FinalResult xs empty
  where
    go ys d =
      case split ys of
        Right (l,_) -> go l $ PartialResult ys d
        _ -> PartialResult ys d

-- | Extract the list of binary lists from a 'Decoded' value.
decodedToList :: Decoded a -> [BinList a]
decodedToList (PartialResult xs d) = xs : decodedToList d
decodedToList (FinalResult xs _) = [xs]
decodedToList (DecodingError _ _) = []

-- | Decode an encoded binary list.
--   The result is given as a 'DecodedBinList' value, which can then be
--   queried to get partial results.
decodeBinList :: Get a -> EncodedBinList -> DecodedBinList a
decodeBinList f (EncodedBinList d l b) = DecodedBinList d l $
  case runGetOrFail f b of
    Left (r,_,err) -> DecodingError err r
    Right (r,_,x) -> go r (ListEnd x)
  where
    -- | Function to get binary trees using the supplied 'Get' value.
    --   The order of the elements depends on the encoding direction.
    --
    -- getBinList :: Exponent-> Get (BinList a)
    getBinList =
       case d of
         FromLeft -> \i -> replicateA  i f
         _        -> \i -> replicateAR i f

    -- | Function to append two binary lists of given length exponent,
    --   where the order of appending depends on the encoding
    --   direction.
    --
    -- recAppend :: Exponent -> BinList a -> BinList a -> BinList a
    recAppend = case d of
       FromLeft -> \i ->        ListNode (i+1)
       _        -> \i -> flip $ ListNode (i+1)

    -- | Recursive decoding function.
    --
    -- go :: ByteString -- ^ Input data.
    --    -> BinList a -- ^ Accumulated binary list.
    --    -> Decoded a
    go input xs =
       let i = lengthExponent xs
       in  if i == l
              -- If the final length exponent has been reached, we stop decoding.
              then FinalResult xs input
              -- Otherwise, we read another chunk of data of the same size of
              -- the already decoded data, prepending the accumulated data as
              -- a partial result.
              else PartialResult xs $ case runGetOrFail (getBinList i) input of
                     -- In case of error, we return a decoding error.
                     Left (r,_,err) -> DecodingError err r
                     Right (r,_,ys) ->
                       let -- The new list is appended with the accumulated list and fed
                           -- to the next recursion step.
                       in  go r $ recAppend i xs ys

-- | Translate an encoded binary list to a bytestring.
encodedToByteString :: EncodedBinList -> ByteString
encodedToByteString (EncodedBinList d l b) = runPut $ do
  -- We start with 0 if the direction is left-to-right, and
  -- with 1 if the direction is right-to-left.
  putWord8 $ if d == FromLeft then 0 else 1
  -- Exponent values are converted to Word64 for backwards compatibility.
  putWord64be $ fromIntegral l
  putLazyByteString b

-- | Translate a bytestring to an encoded binary list, in case this is possible. Otherwise,
--   it returns a string with a human-readable error.
encodedFromByteString :: ByteString -> Either String EncodedBinList
encodedFromByteString input =
  let p = do w <- getWord8
             d <- case w of
                    0 -> return FromLeft
                    1 -> return FromRight
                    _ -> fail $ "encodedFromByteString: unknown direction " ++ show w
             l <- getWord64be
             return (d,l)
  in  case runGetOrFail p input of
        Left (_,_,err) -> Left err
        Right (r,_,(d,l)) -> Right $ EncodedBinList d (fromIntegral l) r

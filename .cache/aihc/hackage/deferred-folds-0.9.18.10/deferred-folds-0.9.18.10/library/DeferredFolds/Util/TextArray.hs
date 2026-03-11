{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module DeferredFolds.Util.TextArray where

import Data.Text.Array
import qualified Data.Text.Internal.Encoding.Utf16 as TextUtf16
import qualified Data.Text.Internal.Unsafe.Char as TextChar
import qualified Data.Text.Unsafe as TextUnsafe
import DeferredFolds.Prelude hiding (Array)

-- |
-- Same as 'Data.Text.Unsafe.iter',
-- but operates on the array directly,
-- uses a continuation and passes the next offset to it instead of delta.
#if MIN_VERSION_text(2,0,0)
{-# INLINE iter #-}
iter :: Array -> Int -> (Char -> Int -> a) -> a
iter arr offset cont =
  let TextUnsafe.Iter c d = TextUnsafe.iterArray arr offset in cont c (offset + d)
#else
{-# INLINE iter #-}
iter :: Array -> Int -> (Char -> Int -> a) -> a
iter arr offset cont =
  let b1 =
        unsafeIndex arr offset
   in if b1 >= 0xd800 && b1 <= 0xdbff
        then
          let b2 =
                unsafeIndex arr (succ offset)
              char =
                TextUtf16.chr2 b1 b2
           in cont char (offset + 2)
        else cont (TextChar.unsafeChr b1) (offset + 1)
#endif

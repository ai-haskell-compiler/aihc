-- | Memory reinterpretation casts for Float\/Double and Word32\/Word64.
--
-- Currently we use the 'array' method from <http://stackoverflow.com/a/7002812/263061>.
--
-- If you need something like `Int32` or similar, `fromIntegral` will do the job
-- within the integral types (so to/from @Word*@ are the only conversions needed).
module Data.ReinterpretCast
  ( Impl.floatToWord
  , Impl.wordToFloat
  , Impl.doubleToWord
  , Impl.wordToDouble
  ) where

import qualified Data.ReinterpretCast.Internal.ImplArray as Impl

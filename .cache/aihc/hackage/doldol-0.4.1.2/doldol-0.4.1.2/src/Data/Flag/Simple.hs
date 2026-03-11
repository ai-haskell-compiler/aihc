{-# LANGUAGE MagicHash, BangPatterns #-}
{-# LANGUAGE CPP #-}

module Data.Flag.Simple
  ( module Data.Flag.Simple
  , module Flag
  ) where


#ifdef DEBUG
import Control.Exception
#endif

import Data.Bits
import Data.Foldable -- For base-4.7.0.*

import GHC.Base


import Data.Flag.Internal as Flag

-- | Encode `Flag` from a given collection of `Enum` e
encodeFlag :: (Foldable f, Bounded e, Enum e) => f e -> Flag
encodeFlag = Data.Foldable.foldr' (\x b -> setBit b (fromEnum x)) zeroBits

-- | Decode `Flag` to a list of `Enum` e
decodeFlag :: Enum e => Flag -> [e]
decodeFlag aFlag = decodeFlagSub (bitLen# -# 1#)
  where
    !(I# bitLen#) = bitLen
    decodeFlagSub (-1#) = []
    decodeFlagSub  idx# =
      if testBit aFlag (I# idx#)
        then toEnum (I# idx#) : decodeFlagSub (idx# -# 1#)
        else                    decodeFlagSub (idx# -# 1#)

-- | Show `Flag` as a binary digit `String`
showFlag :: Flag -> String
showFlag aFlag = showFlagSub (bitLen#-#1#)
  where
    !(I# bitLen#) = bitLen
    showFlagSub (-1#) = ""
    showFlagSub  idx# =
      if testBit aFlag (I# idx#)
        then '1' : showFlagSub (idx#-#1#)
        else '0' : showFlagSub (idx#-#1#)

-- | Show `Flag` as a binary digit `String` with a minimum length
showFlagFit :: (Bounded e, Enum e) => e -> Flag -> String
showFlagFit a aFlag =
#ifdef DEBUG
    -- Assertion for `Flag` according to `a`. This is not needed for `showFlag`.
  assert (isFlaggable a) $
#endif
    showFlagSub bitLen#
  where
    !(I# bitLen#) = fromEnum (maxBound `asTypeOf` a)
    showFlagSub (-1#) = ""
    showFlagSub  idx# =
      if testBit aFlag (I# idx#)
        then '1' : showFlagSub (idx#-#1#)
        else '0' : showFlagSub (idx#-#1#)

-- | Show `Flag` as a binary digit `String` with a given length
showFlagBy :: Int -> Flag -> String
showFlagBy l@(I# bitLen#) aFlag =
#ifdef DEBUG
    -- Assertion for Flag. This is not needed for showFlag
  assert (l <= bitLen && l > 0) $
#endif
    showFlagSub (bitLen#-#1#)
  where
    showFlagSub (-1#) = ""
    showFlagSub  idx# =
      if testBit aFlag (I# idx#)
        then '1' : showFlagSub (idx#-#1#)
        else '0' : showFlagSub (idx#-#1#)

-- | Encode `Flag` from a given binary digit `String`
--
--   This allows any character which is not '0' or '1' as '1'
readFlag :: String -> Flag
readFlag aFlagString = readFlagSub aFlagString zeroBits
readFlagSub [] acc = acc
readFlagSub (x:xs) acc =
  if x == '0'
    then readFlagSub xs next
    else readFlagSub xs (setBit next 0)
  where
    next = shiftL acc 1

-- | Encode `Enum` e from a given binary digit `String`
readEnum :: (Enum e) => String -> [e]
readEnum = decodeFlag . readFlag

-- | Check whether `Flag` f1 covers every positive bit in `Flag` f2
--
--   This implementations implies that when f2 == `zeroBits` then the results of `include` is same as `exclude`
include :: Flag -> Flag -> Bool
include f1 f2 = (f1 .&. f2) == f2
-- | Check whether `Flag` f1 does not cover any positive bit in `Flag` f2
exclude :: Flag -> Flag -> Bool
exclude f1 f2 = (f1 .&. f2) == zeroBits

-- | Apply f with two latter `Flag` arguments under the first `Flag`
about :: (Flag -> Flag -> b) -> Flag -> Flag -> Flag -> b
about f fb f1 f2 = f (fb .&. f1) (fb .&. f2)

-- | Check whether two `Flag`s are same or not under the first `Flag`
--
--   `eqAbout` fb f1 f2 = (fb .&. f1) == (fb .&. f2)
eqAbout :: Flag -> Flag -> Flag -> Bool
eqAbout = about (==)

includeAbout = about include
-- Should be tested that this really works properly!
excludeAbout = about exclude

-- | Check any positive bit of req matches corresponding bit of obj
--
--   When req is `zerobits`, this returns `True`
anyReq :: Flag -> Flag -> Bool
anyReq obj req = (req == zeroBits) || (obj .&. req) /= zeroBits

-- | Check every positive bit of req matches corresponding bit of obj
allReq :: Flag -> Flag -> Bool
allReq obj req = (obj .&. req) == req

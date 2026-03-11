{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

-- Module      : Data.Text.Manipulate.Internal.Fusion
-- Copyright   : (c) 2014-2020 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Data.Text.Manipulate.Internal.Fusion where

import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text.Internal.Fusion as Fusion
import Data.Text.Internal.Fusion.CaseMapping (lowerMapping, upperMapping)
import Data.Text.Internal.Fusion.Common
import Data.Text.Internal.Fusion.Types
import qualified Data.Text.Internal.Lazy.Fusion as LFusion
import qualified Data.Text.Lazy as LText
import Data.Text.Manipulate.Internal.Types

#if MIN_VERSION_text(2,0,0)
import Data.Bits (shiftL, shiftR, (.&.))
import GHC.Exts (Char(..), Int(..), chr#)
import GHC.Int (Int64(..))
#endif

takeWord :: Stream Char -> Stream Char
takeWord = transform (const Done) yield . tokenise
{-# INLINE [0] takeWord #-}

dropWord :: Stream Char -> Stream Char
dropWord (tokenise -> Stream next0 s0 len) = Stream next (True :*: s0) len
  where
    next (skip :*: s) =
      case next0 s of
        Done -> Done
        Skip s' -> Skip (skip :*: s')
        Yield t s' ->
          case t of
            B '\0' -> Skip (False :*: s')
            B _ | skip -> Skip (False :*: s')
            B c -> Yield c (False :*: s')
            _ | skip -> Skip (skip :*: s')
            U c -> Yield c (skip :*: s')
            L c -> Yield c (skip :*: s')
{-# INLINE [0] dropWord #-}

toTitle :: Stream Char -> Stream Char
toTitle = mapHead toUpper . transformWith (yield ' ') upper lower . tokenise
{-# INLINE [0] toTitle #-}

toCamel :: Stream Char -> Stream Char
toCamel = mapHead toLower . transformWith skip' upper lower . tokenise
{-# INLINE [0] toCamel #-}

toPascal :: Stream Char -> Stream Char
toPascal = mapHead toUpper . transformWith skip' upper lower . tokenise
{-# INLINE [0] toPascal #-}

toSnake :: Stream Char -> Stream Char
toSnake = transform (yield '_') lower . tokenise
{-# INLINE [0] toSnake #-}

toSpinal :: Stream Char -> Stream Char
toSpinal = transform (yield '-') lower . tokenise
{-# INLINE [0] toSpinal #-}

toTrain :: Stream Char -> Stream Char
toTrain = mapHead toUpper . transformWith (yield '-') upper lower . tokenise
{-# INLINE [0] toTrain #-}

strict :: (Stream Char -> Stream Char) -> Text -> Text
strict f t = Fusion.unstream (f (Fusion.stream t))
{-# INLINE [0] strict #-}

lazy :: (Stream Char -> Stream Char) -> LText.Text -> LText.Text
lazy f t = LFusion.unstream (f (LFusion.stream t))
{-# INLINE [0] lazy #-}

skip' :: forall s. s -> Step (CC s) Char
#if MIN_VERSION_text(2,0,0)
skip' s = Skip (CC s 0)
#else
skip' s = Skip (CC s '\0' '\0')
#endif

yield, upper, lower :: forall s. Char -> s -> Step (CC s) Char
#if MIN_VERSION_text(2,0,0)

yield !c s = Yield c (CC s 0)

upper !c@(C# c#) s = case I64# (upperMapping c#) of
  0 -> Yield c (CC s 0)
  ab -> let (a, b) = chopOffChar ab in
              Yield a (CC s b)

lower !c@(C# c#) s = case I64# (lowerMapping c#) of
  0 -> Yield c (CC s 0)
  ab -> let (a, b) = chopOffChar ab in
              Yield a (CC s b)

chopOffChar :: Int64 -> (Char, Int64)
chopOffChar ab = (chr a, ab `shiftR` 21)
  where
    chr (I# n) = C# (chr# n)
    mask = (1 `shiftL` 21) - 1
    a = fromIntegral $ ab .&. mask

#else

yield !c s = Yield c (CC s '\0' '\0')
upper !c s = upperMapping c s
lower !c s = lowerMapping c s

#endif

-- | Step across word boundaries using a custom action, and transform
-- both subsequent uppercase and lowercase characters uniformly.
--
-- /See:/ 'transformWith'
transform ::
  -- | Boundary action.
  (forall s. s -> Step (CC s) Char) ->
  -- | Character mapping.
  (forall s. Char -> s -> Step (CC s) Char) ->
  -- | Input stream.
  Stream Token ->
  Stream Char
transform s m = transformWith s m m
{-# INLINE [0] transform #-}

-- | Step across word boundaries using a custom action, and transform
-- subsequent characters after the word boundary is encountered with a mapping
-- depending on case.
transformWith ::
  -- | Boundary action.
  (forall s. s -> Step (CC s) Char) ->
  -- | Boundary mapping.
  (forall s. Char -> s -> Step (CC s) Char) ->
  -- | Subsequent character mapping.
  (forall s. Char -> s -> Step (CC s) Char) ->
  -- | Input stream.
  Stream Token ->
  Stream Char
transformWith md mu mc (Stream next0 s0 len) =
  -- HINT: len incorrect when the boundary replacement yields a char.
#if MIN_VERSION_text(2,0,0)
  Stream next (CC (False :*: False :*: s0) 0) len
#else
  Stream next (CC (False :*: False :*: s0) '\0' '\0') len
#endif
  where
#if MIN_VERSION_text(2,0,0)
    next (CC (up :*: prev :*: s) 0) =
#else
    next (CC (up :*: prev :*: s) '\0' _) =
#endif
      case next0 s of
        Done -> Done
#if MIN_VERSION_text(2,0,0)
        Skip s' -> Skip (CC (up :*: prev :*: s') 0)
#else
        Skip s' -> Skip (CC (up :*: prev :*: s') '\0' '\0')
#endif
        Yield t s' ->
          case t of
            B _ -> md (False :*: True :*: s')
            U c | prev -> mu c (True :*: False :*: s')
            L c | prev -> mu c (False :*: False :*: s')
            U c | up -> mu c (True :*: False :*: s')
            U c -> mc c (True :*: False :*: s')
            L c -> mc c (False :*: False :*: s')
#if MIN_VERSION_text(2,0,0)
    next (CC s ab) = let (a, b) = chopOffChar ab in Yield a (CC s b)
#else
    next (CC s a b) = Yield a (CC s b '\0')
#endif
{-# INLINE [0] transformWith #-}

-- | A token representing characters and boundaries in a stream.
data Token
  = -- | Word boundary.
    B {-# UNPACK #-} !Char
  | -- | Upper case character.
    U {-# UNPACK #-} !Char
  | -- | Lower case character.
    L {-# UNPACK #-} !Char
  deriving (Show)

-- | Tokenise a character stream using the default 'isBoundary' predicate.
--
-- /See:/ 'tokeniseWith'
tokenise ::
  -- | Input stream.
  Stream Char ->
  Stream Token
tokenise = tokeniseWith isBoundary
{-# INLINE [0] tokenise #-}

-- | Tokenise a character stream using a custom boundary predicate.
tokeniseWith ::
  -- | Boundary predicate.
  (Char -> Bool) ->
  -- | Input stream.
  Stream Char ->
  Stream Token
tokeniseWith f (Stream next0 s0 len) =
  -- HINT: len incorrect if there are adjacent boundaries, which are skipped.
#if MIN_VERSION_text(2,0,0)
  Stream next (CC (True :*: False :*: False :*: s0) 0) len
#else
  Stream next (CC (True :*: False :*: False :*: s0) '\0' '\0') len
#endif
  where
#if MIN_VERSION_text(2,0,0)
    next (CC (start :*: up :*: prev :*: s) 0) =
#else
    next (CC (start :*: up :*: prev :*: s) '\0' _) =
#endif
      case next0 s of
        Done -> Done
#if MIN_VERSION_text(2,0,0)
        Skip s' -> Skip (CC (start :*: up :*: prev :*: s') 0)
#else
        Skip s' -> Skip (CC (start :*: up :*: prev :*: s') '\0' '\0')
#endif
        Yield c s'
          | not b, start -> push
          | up -> push
          | b, prev -> Skip (step start)
          | otherwise -> push
          where
            push
              | b = Yield (B c) (step False)
              | u, skip = Yield (U c) (step False)
#if MIN_VERSION_text(2,0,0)
              | u = Yield (B '\0') (CC (False :*: u :*: b :*: s') (fromIntegral (Char.ord c)))
#else
              | u = Yield (B '\0') (CC (False :*: u :*: b :*: s') c '\0')
#endif
              | otherwise = Yield (L c) (step False)

#if MIN_VERSION_text(2,0,0)
            step p = CC (p :*: u :*: b :*: s') 0
#else
            step p = CC (p :*: u :*: b :*: s') '\0' '\0'
#endif

            skip = up || start || prev

            b = f c
            u = Char.isUpper c
#if MIN_VERSION_text(2,0,0)
    next (CC s ab) = let (a, b) = chopOffChar ab in Yield (U a) (CC s b)
#else
    next (CC s a b) = Yield (U a) (CC s b '\0')
#endif
{-# INLINE [0] tokeniseWith #-}

mapHead :: (Stream Char -> Stream Char) -> Stream Char -> Stream Char
mapHead f s = maybe s (\(x, s') -> f (singleton x) `append` s') (uncons s)
{-# INLINE [0] mapHead #-}

-- We use -XMagicHash instead of the LANGUAGE pragma since we can't
-- CPP-guard language extensions on older versions of GHC.
{-# OPTIONS_GHC -Wall -fwarn-tabs -XMagicHash #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
-- N.B., GHC.Exts isn't "safe".
{-# LANGUAGE Trustworthy #-}
#endif
----------------------------------------------------------------
--                                                    2021.10.17
-- |
-- Module      :  Prelude.SafeEnum
-- Copyright   :  2012--2021 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@cpan.org
-- Stability   :  provisional
-- Portability :  Haskell98 + CPP (+ MagicHash)
--
-- A redefinition of the "Prelude"\'s 'Prelude.Enum' class in order
-- to render it safe. That is, the Haskell Language Report defines
-- 'Prelude.pred', 'Prelude.succ', 'Prelude.fromEnum' and
-- 'Prelude.toEnum' to be partial functions when the type is
-- 'Bounded'[1], but this is unacceptable. So these classes are
-- offered as a replacement, correcting the types of those functions.
-- We intentionally clash with the names of the Prelude's class;
-- if you wish to use both in the same file, then import this module
-- (or the Prelude) qualified.
--
-- While we're at it, we also generalize the notion of enumeration.
-- Rather than requiring that the type is linearly enumerable, we
-- distinguish between forward enumeration (which allows for multiple
-- predecessors) and backward enumeration (which allows for multiple
-- successors). Moreover, we do not require that the enumeration
-- order coincides with the 'Ord' ordering (if one exists), though
-- it's advisable that they do (for your sanity). However, we also
-- ensure that the notion of enumeration (in either direction) is
-- well-defined, which rules out instances for 'Float' and 'Double',
-- and renders instances for 'Data.Ratio.Ratio' problematic.
-- 'Data.Ratio.Ratio' instances /can/ be provided so long as the
-- base type is integral and enumerable; but they must be done in
-- an obscure order[2] that does not coincide with 'Ord'. Since
-- this is not what people may expect, we only provide an instance
-- for the newtype 'Data.Number.CalkinWilf.CalkinWilf', not for
-- 'Data.Ratio.Ratio' itself.
--
-- The @MagicHash@ extension is only actually required if on GHC.
-- This extension is used only so that the implementation of the
-- instances for 'Char' match those of the Prelude's 'Prelude.Enum'.
-- I have not benchmarked to determine whether this low-level hackery
-- is actually still necessary.
--
-- [1] <http://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1310006.3.4>
--
-- [2] Jeremy Gibbons, David Lester, and Richard Bird (2006).
--     /Enumerating the Rationals/. JFP 16(3):281--291.
--     DOI:10.1017\/S0956796806005880
--     <http://www.cs.ox.ac.uk/jeremy.gibbons/publications/rationals.pdf>
----------------------------------------------------------------
module Prelude.SafeEnum
    ( UpwardEnum(..)
    , DownwardEnum(..)
    , Enum(..)
    ) where

import Prelude hiding (Enum(..))
import qualified Prelude (Enum(..))

#ifdef __GLASGOW_HASKELL__
#   if __GLASGOW_HASKELL__ >= 708
import GHC.Exts (isTrue#, (/=#))
#   else
import GHC.Exts ((==#))
#   endif
import GHC.Exts (build, Int(I#), Char(C#), ord#, chr#, (<=#), (+#), (-#), leChar#)
#else
import Data.Char (chr, ord)
#endif
----------------------------------------------------------------
----------------------------------------------------------------
infix 4 `precedes`, `succeeds`


----------------------------------------------------------------
-- | A class for upward enumerable types. That is, we can enumerate
-- larger and larger values, eventually getting every one of them;
-- i.e., given any @x@, for all @y@ such that @y \`succeeds\` x@,
-- it must be the case that @y@ occurs within some finite prefix
-- of @enumFrom x@.
--
-- We require that 'succeeds' forms a strict partial order. That
-- is, it must obey the following laws (N.B., if the first two laws
-- hold, then the third one follows for free):
--
-- > if x `succeeds` y && y `succeeds` z then x `succeeds` z
-- > if x `succeeds` y then not (y `succeeds` x)
-- > not (x `succeeds` x)
--
-- Moreover, we require that 'succeeds' agrees with 'succ', and
-- that 'succ' is exhaustive for 'succeeds' (assuming @Eq a@, by
-- magic if need be):
--
-- > if succ x == Just y then y `succeeds` x
-- > if x `succeeds` y   then x `elem` enumFrom y
--
-- Minimal complete definition: 'succ', 'succeeds'.
class UpwardEnum a where

    -- | The successor of a value, or @Nothing@ is there isn't one.
    -- For the numeric types in the Prelude, 'succ' adds 1.
    succ :: a -> Maybe a

    -- | A variant of @('>')@ with regards to the enumeration order.
    succeeds :: a -> a -> Bool

    -- converse well-founded ~ Noetherian
    -- | Return @x@ followed by all it's successors, in order. The
    -- resulting list is always non-empty, since it includes @x@.
    -- If the resulting list is always finite, then the 'succeeds'
    -- ordering is converse well-founded. In GHC, the default
    -- implementation is a \"good producer\" for list fusion.
    enumFrom :: a -> [a]
#ifdef __GLASGOW_HASKELL__
    {-# INLINE enumFrom #-}
    enumFrom x0 = build (enumFromFB x0)
        where
        {-# INLINE [0] enumFromFB #-}
        enumFromFB x cons nil = x `cons`
            case succ x of
            Nothing -> nil
            Just y  -> enumFromFB y cons nil
#else
    enumFrom x = x :
        case succ x of
        Nothing -> []
        Just y  -> enumFrom y
#endif

    -- | Return the elements of @'enumFrom' x@, filtering out
    -- everything that succeeds @z@. If @x@ succeeds @z@, then the
    -- resulting list is empty; otherwise, it is non-empty, since
    -- it includes @x@. In GHC, the default implementation is a
    -- \"good producer\" for list fusion.
    enumFromTo :: a -> a -> [a]
#ifdef __GLASGOW_HASKELL__
    {-# INLINE enumFromTo #-}
    enumFromTo x0 z0 = build (enumFromToFB x0 z0)
        where
        {-# INLINE [0] enumFromToFB #-}
        enumFromToFB x z cons nil
            |  x `succeeds` z = nil
            | otherwise       = x `cons`
                case succ x of
                Nothing -> nil
                Just y  -> enumFromToFB y z cons nil
#else
    enumFromTo x z
        |  x `succeeds` z = []
        | otherwise       = x :
            case succ x of
            Nothing -> []
            Just y  -> enumFromTo y z
#endif


----------------------------------------------------------------
-- | A class for downward enumerable types. That is, we can enumerate
-- smaller and smaller values, eventually getting every one of them;
-- i.e., given any @x@, for all @y@ such that @y \`precedes\` x@,
-- it must be the case that @y@ occurs within some finite prefix
-- of @enumDownFrom x@.
--
-- We require that 'precedes' forms a strict partial order. That
-- is, it must obey the following laws (N.B., if the first two laws
-- hold, then the third one follows for free):
--
-- > if x `precedes` y && y `precedes` z then x `precedes` z
-- > if x `precedes` y then not (y `precedes` x)
-- > not (x `precedes` x)
--
-- Moreover, we require that 'precedes' agrees with 'pred', and
-- that 'pred' is exhaustive for 'precedes' (assuming @Eq a@, by
-- magic if need be):
--
-- > if pred x == Just y then y `precedes` x
-- > if x `precedes` y   then x `elem` enumDownFrom y
--
-- Minimal complete definition: 'pred', 'precedes'.
class DownwardEnum a where

    -- | The predecessor of a value, or @Nothing@ is there isn't one.
    -- For the numeric types in the Prelude, 'pred' subtracts 1.
    pred :: a -> Maybe a

    -- | A variant of @('<')@ with regards to the enumeration order.
    precedes :: a -> a -> Bool

    -- well-founded ~ Artinian
    -- | Return @x@ followed by all it's predecessors, in (reverse)
    -- order. The resulting list is always non-empty, since it
    -- includes @x@. If the resulting list is always finite, then
    -- the 'precedes' ordering is well-founded. In GHC, the default
    -- implementation is a \"good producer\" for list fusion.
    enumDownFrom :: a -> [a]
#ifdef __GLASGOW_HASKELL__
    {-# INLINE enumDownFrom #-}
    enumDownFrom x0 = build (enumDownFromFB x0)
        where
        {-# INLINE [0] enumDownFromFB #-}
        enumDownFromFB x cons nil = x `cons`
            case pred x of
            Nothing -> nil
            Just y  -> enumDownFromFB y cons nil
#else
    enumDownFrom x = x :
        case pred x of
        Nothing -> []
        Just y  -> enumDownFrom y
#endif

    -- | Return the elements of @'enumDownFrom' x@, filtering out
    -- everything that precedes @z@. If @x@ precedes @z@, then the
    -- resulting list is empty; otherwise, it is non-empty, since
    -- it includes @x@. In GHC, the default implementation is a
    -- \"good producer\" for list fusion.
    enumDownFromTo :: a -> a -> [a]
#ifdef __GLASGOW_HASKELL__
    {-# INLINE enumDownFromTo #-}
    enumDownFromTo x0 z0 = build (enumDownFromToFB x0 z0)
        where
        {-# INLINE [0] enumDownFromToFB #-}
        enumDownFromToFB x z cons nil
            |  x `precedes` z = nil
            | otherwise       = x `cons`
                case pred x of
                Nothing -> nil
                Just y  -> enumDownFromToFB y z cons nil
#else
    enumDownFromTo x z
        |  x `precedes` z = []
        | otherwise       = x :
            case pred x of
            Nothing -> []
            Just y  -> enumDownFromTo y z
#endif

----------------------------------------------------------------
-- | A class for types with a linear enumeration order. We require
-- that the partial orders of the superclasses agree:
--
-- > x `succeeds` y  ==  y `precedes` x
--
-- That the enumeration order is preserved\/reflected:
--
-- > i `succeeds` j  ==  toEnum   i `succeeds` toEnum   j
-- > x `succeeds` y  ==  fromEnum x `succeeds` fromEnum y
--
-- And that 'toEnum' and 'fromEnum' form a weak isomorphism; i.e.,
-- for some @p@ and @q@, the following must hold:
--
-- > fromEnum <=< toEnum    ==  (\i -> if p i then Just i else Nothing)
-- > toEnum   <=< fromEnum  ==  (\x -> if q x then Just x else Nothing)
--
-- In other words, the following type-restricted functions form an
-- isomorphism of linear orderings.
--
-- > toEnum'   :: {i :: Int | toEnum   i == Just _} -> a
-- > fromEnum' :: {x :: a   | fromEnum x == Just _} -> Int
--
-- Minimal complete definition: 'toEnum', 'fromEnum'. N.B., the
-- default definitions for 'enumFromThen' and 'enumFromThenTo' only
-- make sense when the type @a@ is \"smaller\" than 'Int' (i.e.,
-- 'fromEnum' always succeeds); if 'fromEnum' ever fails, then you
-- must override the defaults in order to correctly infer the stride
-- for values which cannot be converted to 'Int'.
class (UpwardEnum a, DownwardEnum a) => Enum a where

    -- | Convert from an 'Int'.
    toEnum :: Int -> Maybe a

    -- | Convert to an 'Int'.
    fromEnum :: a -> Maybe Int


    -- | Enumerate values with an inferred stride. The resulting
    -- list is always non-empty, since it includes @x@. Naturally,
    -- this should agree with 'enumFrom' and 'enumDownFrom' (assuming
    -- @Eq a@, by magic if need be):
    --
    -- > if succ x == Just y then enumFromThen x y == enumFrom x
    -- > if pred x == Just y then enumFromThen x y == enumDownFrom x
    --
    -- In the default implementation: if 'fromEnum' fails on either
    -- argument, then the result is exactly @[x]@; and if 'toEnum'
    -- fails on any of the enumerated integers, then the first
    -- failure terminates the enumeration. If either of these
    -- properties is inappropriate, then you should override the
    -- default. In GHC, the default implementation is a \"good
    -- producer\" for list fusion.
    enumFromThen :: a -> a -> [a]
    enumFromThen x y =
        maybe [x] id $! do
            x' <- fromEnum x
            y' <- fromEnum y
            return $! takeEnum (enumFromThen x' y')


    -- | Enumerate values with an inferred stride and a given limit.
    -- If @x@ precedes @y@ (and therefore we're enumerating forward)
    -- but @x@ succeeds @z@ (and therefore is past the limit), then
    -- the result is empty. Similarly, if @x@ succeeds @y@ (and
    -- therefore we're enumerating backward) but @x@ precedes @z@
    -- (and therefore is past the limit), then the result is empty.
    -- Otherwise the result is non-empty since it contains @x@.
    -- Naturally, this should agree with 'enumFromTo' and
    -- 'enumDownFromTo' (assuming @Eq a@, by magic if need be):
    --
    -- > if succ x == Just y then enumFromThenTo x y z == enumFromTo x z
    -- > if pred x == Just y then enumFromThenTo x y z == enumDownFromTo x z
    --
    -- In the default implementation: if 'fromEnum' fails on any
    -- argument, then the result is either @[]@ or @[x]@ (as
    -- appropriate); and if 'toEnum' fails on any of the enumerated
    -- integers, then the first failure terminates the enumeration.
    -- If either of these properties is inappropriate, then you
    -- should override the default. In GHC, the default implementation
    -- is a \"good producer\" for list fusion.
    enumFromThenTo :: a -> a -> a -> [a]
    enumFromThenTo x y z
        | x `precedes` y && x `succeeds` z = []
        | x `succeeds` y && x `precedes` z = []
        | otherwise =
            maybe [x] id $! do
                x' <- fromEnum x
                y' <- fromEnum y
                z' <- fromEnum z
                return $! takeEnum (enumFromThenTo x' y' z')


-- | Convert the integers via 'toEnum', and keep taking the results
-- until the first @Nothing@. In GHC, this is both a \"good producer\"
-- and a \"good consumer\" for list fusion.
takeEnum :: Enum a => [Int] -> [a]
#ifdef __GLASGOW_HASKELL__
{-# INLINE takeEnum #-}
takeEnum xs0 = build (\cons nil -> foldr (takeEnumCons cons nil) nil xs0)
    where
    {-# INLINE takeEnumCons #-}
    takeEnumCons cons nil x ys =
        case toEnum x of
        Nothing -> nil
        Just y  -> y `cons` ys
#else
takeEnum []     = []
takeEnum (x:xs) =
    case toEnum x of
    Nothing -> []
    Just y  -> y : takeEnum xs
#endif

----------------------------------------------------------------
----------------------------------------------------------------
----- Inherited instances from the Prelude

preludeEnumDownFrom :: (DownwardEnum a, Prelude.Enum a) => a -> [a]
preludeEnumDownFrom x =
    case pred x of
    Nothing -> [x]
    Just y  -> Prelude.enumFromThen x y
{-# INLINE preludeEnumDownFrom #-}


preludeEnumDownFromTo :: (DownwardEnum a, Prelude.Enum a) => a -> a -> [a]
preludeEnumDownFromTo x z =
    case pred x of
    Nothing -> [x]
    Just y  -> Prelude.enumFromThenTo x y z
{-# INLINE preludeEnumDownFromTo #-}


instance UpwardEnum () where
    succ ()    = Nothing
    succeeds   = (>)
    enumFrom   = Prelude.enumFrom
    enumFromTo = Prelude.enumFromTo
    {-# INLINE succ #-}
    {-# INLINE succeeds #-}
    {-# INLINE enumFrom #-}
    {-# INLINE enumFromTo #-}

instance DownwardEnum () where
    pred ()        = Nothing
    precedes       = (<)
    enumDownFrom   = preludeEnumDownFrom
    enumDownFromTo = preludeEnumDownFromTo
    {-# INLINE pred #-}
    {-# INLINE precedes #-}
    {-# INLINE enumDownFrom #-}
    {-# INLINE enumDownFromTo #-}

instance Enum () where
    toEnum i
        | i == 0    = Just ()
        | otherwise = Nothing
    fromEnum ()     = Just 0
    enumFromThen    = Prelude.enumFromThen
    enumFromThenTo  = Prelude.enumFromThenTo
    {-# INLINE toEnum #-}
    {-# INLINE fromEnum #-}
    {-# INLINE enumFromThen #-}
    {-# INLINE enumFromThenTo #-}

----------------------------------------------------------------
instance UpwardEnum Bool where
    succ False = Just True
    succ True  = Nothing
    succeeds   = (>)
    enumFrom   = Prelude.enumFrom
    enumFromTo = Prelude.enumFromTo
    {-# INLINE succ #-}
    {-# INLINE succeeds #-}
    {-# INLINE enumFrom #-}
    {-# INLINE enumFromTo #-}

instance DownwardEnum Bool where
    pred True      = Just False
    pred False     = Nothing
    precedes       = (<)
    enumDownFrom   = preludeEnumDownFrom
    enumDownFromTo = preludeEnumDownFromTo
    {-# INLINE pred #-}
    {-# INLINE precedes #-}
    {-# INLINE enumDownFrom #-}
    {-# INLINE enumDownFromTo #-}

instance Enum Bool where
    toEnum i
        | i == 0    = Just False
        | i == 1    = Just True
        | otherwise = Nothing
    fromEnum False  = Just 0
    fromEnum True   = Just 1
    enumFromThen    = Prelude.enumFromThen
    enumFromThenTo  = Prelude.enumFromThenTo
    {-# INLINE toEnum #-}
    {-# INLINE fromEnum #-}
    {-# INLINE enumFromThen #-}
    {-# INLINE enumFromThenTo #-}

----------------------------------------------------------------
instance UpwardEnum Ordering where
    succ LT    = Just EQ
    succ EQ    = Just GT
    succ GT    = Nothing
    succeeds   = (>)
    enumFrom   = Prelude.enumFrom
    enumFromTo = Prelude.enumFromTo
    {-# INLINE succ #-}
    {-# INLINE succeeds #-}
    {-# INLINE enumFrom #-}
    {-# INLINE enumFromTo #-}

instance DownwardEnum Ordering where
    pred GT        = Just EQ
    pred EQ        = Just LT
    pred LT        = Nothing
    precedes       = (<)
    enumDownFrom   = preludeEnumDownFrom
    enumDownFromTo = preludeEnumDownFromTo
    {-# INLINE pred #-}
    {-# INLINE precedes #-}
    {-# INLINE enumDownFrom #-}
    {-# INLINE enumDownFromTo #-}

instance Enum Ordering where
    toEnum i
        | i == 0    = Just LT
        | i == 1    = Just EQ
        | i == 2    = Just GT
        | otherwise = Nothing
    fromEnum LT     = Just 0
    fromEnum EQ     = Just 1
    fromEnum GT     = Just 2
    enumFromThen    = Prelude.enumFromThen
    enumFromThenTo  = Prelude.enumFromThenTo
    {-# INLINE toEnum #-}
    {-# INLINE fromEnum #-}
    {-# INLINE enumFromThen #-}
    {-# INLINE enumFromThenTo #-}

----------------------------------------------------------------
instance UpwardEnum Char where
#if __GLASGOW_HASKELL__ >= 708
    succ (C# c#)
        | isTrue# (ord# c# /=# 0x10FFFF#)
                    = Just $! C# (chr# (ord# c# +# 1#))
        | otherwise = Nothing
#elif defined __GLASGOW_HASKELL__
    succ (C# c#)
        | not (ord# c# ==# 0x10FFFF#) = Just $! C# (chr# (ord# c# +# 1#))
        | otherwise                   = Nothing
#else
    succ c
        | not (ord c == 0x10FFFF) = Just $! chr (ord c + 1)
        | otherwise               = Nothing
#endif
    succeeds   = (>)
    enumFrom   = Prelude.enumFrom
    enumFromTo = Prelude.enumFromTo
    {-# INLINE succ #-}
    {-# INLINE succeeds #-}
    {-# INLINE enumFrom #-}
    {-# INLINE enumFromTo #-}

instance DownwardEnum Char where
#if __GLASGOW_HASKELL__ >= 708
    pred (C# c#)
        | isTrue# (ord# c# /=# 0#)
                    = Just $! C# (chr# (ord# c# -# 1#))
        | otherwise = Nothing
#elif defined __GLASGOW_HASKELL__
    pred (C# c#)
        | not (ord# c# ==# 0#) = Just $! C# (chr# (ord# c# -# 1#))
        | otherwise            = Nothing
#else
    pred c
        | not (ord c == 0) = Just $! chr (ord c - 1)
        | otherwise        = Nothing
#endif
    precedes       = (<)
    enumDownFrom   = preludeEnumDownFrom
    enumDownFromTo = preludeEnumDownFromTo
    {-# INLINE pred #-}
    {-# INLINE precedes #-}
    {-# INLINE enumDownFrom #-}
    {-# INLINE enumDownFromTo #-}

instance Enum Char where
#if __GLASGOW_HASKELL__ >= 708
    toEnum (I# i#)
        | isTrue# (0# <=# i#)
            && isTrue# (i# <=# 0x10FFFF#) = Just $! C# (chr# i#)
        | otherwise                       = Nothing
    fromEnum (C# c#)
        | isTrue# (leChar# (chr# 0#) c#)
            && isTrue# (leChar# c# (chr# 0x10FFFF#)) = Just $! I# (ord# c#)
        | otherwise                                  = Nothing
#elif defined __GLASGOW_HASKELL__
    toEnum (I# i#)
        | 0# <=# i# && i# <=# 0x10FFFF# = Just $! C# (chr# i#)
        | otherwise                     = Nothing
    fromEnum (C# c#)
        | leChar# (chr# 0#) c#
            && leChar# c# (chr# 0x10FFFF#) = Just $! I# (ord# c#)
        | otherwise                        = Nothing
#else
    toEnum i
        | 0 <= i && i <= 0x10FFFF = Just $! chr i
        | otherwise               = Nothing
    fromEnum c
        | chr 0 <= c && c <= chr 0x10FFFF = Just $! ord c
        | otherwise                       = Nothing
#endif
    enumFromThen   = Prelude.enumFromThen
    enumFromThenTo = Prelude.enumFromThenTo
    {-# INLINE toEnum #-}
    {-# INLINE fromEnum #-}
    {-# INLINE enumFromThen #-}
    {-# INLINE enumFromThenTo #-}

----------------------------------------------------------------
instance UpwardEnum Int where
    succ x
        | x == maxBound = Nothing
        | otherwise     = Just $! x + 1
    succeeds   = (>)
    enumFrom   = Prelude.enumFrom
    enumFromTo = Prelude.enumFromTo
    {-# INLINE succ #-}
    {-# INLINE succeeds #-}
    {-# INLINE enumFrom #-}
    {-# INLINE enumFromTo #-}

instance DownwardEnum Int where
    pred x
        | x == minBound = Nothing
        | otherwise     = Just $! x - 1
    precedes       = (<)
    enumDownFrom   = preludeEnumDownFrom
    enumDownFromTo = preludeEnumDownFromTo
    {-# INLINE pred #-}
    {-# INLINE precedes #-}
    {-# INLINE enumDownFrom #-}
    {-# INLINE enumDownFromTo #-}

instance Enum Int where
    toEnum         = Just
    fromEnum       = Just
    enumFromThen   = Prelude.enumFromThen
    enumFromThenTo = Prelude.enumFromThenTo
    {-# INLINE toEnum #-}
    {-# INLINE fromEnum #-}
    {-# INLINE enumFromThen #-}
    {-# INLINE enumFromThenTo #-}

----------------------------------------------------------------
-- TODO: instances for Int8, Int16, Int32, Int64

----------------------------------------------------------------
-- TODO: instances for Word, Word8, Word16, Word32, Word64

----------------------------------------------------------------
instance UpwardEnum Integer where
    succ n     = Just $! n + 1
    succeeds   = (>)
    enumFrom   = Prelude.enumFrom
    enumFromTo = Prelude.enumFromTo
    {-# INLINE succ #-}
    {-# INLINE succeeds #-}
    {-# INLINE enumFrom #-}
    {-# INLINE enumFromTo #-}

instance DownwardEnum Integer where
    pred n         = Just $! n - 1
    precedes       = (<)
    enumDownFrom   = preludeEnumDownFrom
    enumDownFromTo = preludeEnumDownFromTo
    {-# INLINE pred #-}
    {-# INLINE precedes #-}
    {-# INLINE enumDownFrom #-}
    {-# INLINE enumDownFromTo #-}

instance Enum Integer where
    toEnum   n = Just $! Prelude.toEnum n
    fromEnum n
        | min_ <= n && n <= max_ = Just $! Prelude.fromEnum n
        | otherwise              = Nothing
        where
        min_ = fromIntegral (minBound :: Int)
        max_ = fromIntegral (maxBound :: Int)
    enumFromThen   = Prelude.enumFromThen
    enumFromThenTo = Prelude.enumFromThenTo
    {-# INLINE toEnum #-}
    {-# INLINE fromEnum #-}
    {-# INLINE enumFromThen #-}
    {-# INLINE enumFromThenTo #-}

----------------------------------------------------------------
{-
-- The 'succ'\/'pred' functions are not complete for 'succeeds'\/'precedes'

instance UpwardEnum Float where
    succ n     = Just $! n + 1
    succeeds   = (>)
    enumFrom   = Prelude.enumFrom
    enumFromTo = Prelude.enumFromTo
    {-# INLINE succ #-}
    {-# INLINE succeeds #-}
    {-# INLINE enumFrom #-}
    {-# INLINE enumFromTo #-}

instance DownwardEnum Float where
    pred n         = Just $! n - 1
    precedes       = (<)
    enumDownFrom   = preludeEnumDownFrom
    enumDownFromTo = preludeEnumDownFromTo
    {-# INLINE pred #-}
    {-# INLINE precedes #-}
    {-# INLINE enumDownFrom #-}
    {-# INLINE enumDownFromTo #-}

instance Enum Float where
    toEnum   n     = Just $! Prelude.toEnum   n -- Does int2Float ever error?
    fromEnum n     = Just $! Prelude.fromEnum n -- Does fromInteger . truncate?
    enumFromThen   = Prelude.enumFromThen
    enumFromThenTo = Prelude.enumFromThenTo
    {-# INLINE toEnum #-}
    {-# INLINE fromEnum #-}
    {-# INLINE enumFromThen #-}
    {-# INLINE enumFromThenTo #-}

----------------------------------------------------------------
instance UpwardEnum Double where
    succ n     = Just $! n + 1
    succeeds   = (>)
    enumFrom   = Prelude.enumFrom
    enumFromTo = Prelude.enumFromTo
    {-# INLINE succ #-}
    {-# INLINE succeeds #-}
    {-# INLINE enumFrom #-}
    {-# INLINE enumFromTo #-}

instance DownwardEnum Double where
    pred n         = Just $! n - 1
    precedes       = (<)
    enumDownFrom   = preludeEnumDownFrom
    enumDownFromTo = preludeEnumDownFromTo
    {-# INLINE pred #-}
    {-# INLINE precedes #-}
    {-# INLINE enumDownFrom #-}
    {-# INLINE enumDownFromTo #-}

instance Enum Double where
    toEnum   n     = Just $! Prelude.toEnum   n -- Does int2Double ever error?
    fromEnum n     = Just $! Prelude.fromEnum n -- Does fromInteger . truncate?
    enumFromThen   = Prelude.enumFromThen
    enumFromThenTo = Prelude.enumFromThenTo
    {-# INLINE toEnum #-}
    {-# INLINE fromEnum #-}
    {-# INLINE enumFromThen #-}
    {-# INLINE enumFromThenTo #-}
-}


----------------------------------------------------------------
-- TODO: the following (as appropriate)
{-
instance Enum (Data.Fixed.Fixed a)

instance Enum System.IO.IOMode
instance Enum GHC.IO.Device.SeekMode
instance Enum Foreign.C.Types.CUIntMax
instance Enum Foreign.C.Types.CIntMax
instance Enum Foreign.C.Types.CUIntPtr
instance Enum Foreign.C.Types.CIntPtr
instance Enum Foreign.C.Types.CSUSeconds
instance Enum Foreign.C.Types.CUSeconds
instance Enum Foreign.C.Types.CTime
instance Enum Foreign.C.Types.CClock
instance Enum Foreign.C.Types.CSigAtomic
instance Enum Foreign.C.Types.CWchar
instance Enum Foreign.C.Types.CSize
instance Enum Foreign.C.Types.CPtrdiff
instance Enum Foreign.C.Types.CDouble
instance Enum Foreign.C.Types.CFloat
instance Enum Foreign.C.Types.CULLong
instance Enum Foreign.C.Types.CLLong
instance Enum Foreign.C.Types.CULong
instance Enum Foreign.C.Types.CLong
instance Enum Foreign.C.Types.CUInt
instance Enum Foreign.C.Types.CInt
instance Enum Foreign.C.Types.CUShort
instance Enum Foreign.C.Types.CShort
instance Enum Foreign.C.Types.CUChar
instance Enum Foreign.C.Types.CSChar
instance Enum Foreign.C.Types.CChar
instance Enum Data.Char.GeneralCategory
instance Enum Foreign.Ptr.IntPtr
instance Enum Foreign.Ptr.WordPtr
instance Enum System.Posix.Types.Fd
instance Enum System.Posix.Types.CRLim
instance Enum System.Posix.Types.CTcflag
instance Enum System.Posix.Types.CSpeed
instance Enum System.Posix.Types.CCc
instance Enum System.Posix.Types.CUid
instance Enum System.Posix.Types.CNlink
instance Enum System.Posix.Types.CGid
instance Enum System.Posix.Types.CSsize
instance Enum System.Posix.Types.CPid
instance Enum System.Posix.Types.COff
instance Enum System.Posix.Types.CMode
instance Enum System.Posix.Types.CIno
instance Enum System.Posix.Types.CDev
-}

----------------------------------------------------------------
----------------------------------------------------------- fin.

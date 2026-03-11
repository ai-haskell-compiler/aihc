{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE CPP #-}
----------------------------------------------------------------
--                                                    2021.10.17
-- |
-- Module      :  Data.Or
-- Copyright   :  Copyright (c) 2010--2021 wren gayle romano
-- License     :  BSD
-- Maintainer  :  wren@cpan.org
-- Stability   :  provisional
-- Portability :  Haskell98 + CPP
--
-- A data type for non-exclusive disjunction. This is helpful for
-- things like a generic merge function on sets\/maps which could
-- be union, mutual difference, etc. based on which 'Or' value a
-- function argument returns. Also useful for non-truncating zips
-- (cf. 'zipOr') and other cases where you sometimes want an 'Either'
-- and sometimes want a pair.
----------------------------------------------------------------
module Data.Or
    ( Or(Fst,Both,Snd), elimOr, eitherOr
    -- * Non-truncating zipping functions
    , zipOr, zipOrWith, zipOrBy, zipOrWithBy
    ) where

#ifdef __GLASGOW_HASKELL__
import GHC.Base (build)
#endif
----------------------------------------------------------------

-- | A data type for non-exclusive disjunction.
data Or a b = Fst a | Both a b | Snd b
    deriving (Read, Show, Eq)


-- | Functional eliminator for 'Or'.
elimOr :: (a -> c) -> (a -> b -> c) -> (b -> c) -> Or a b -> c
elimOr f _ _ (Fst  a)   = f a
elimOr _ g _ (Both a b) = g a b
elimOr _ _ h (Snd    b) = h   b
{-# INLINE elimOr #-}


-- | Convert an 'Either' into an 'Or'.
eitherOr :: Either a b -> Or a b
eitherOr (Left  a) = Fst a
eitherOr (Right b) = Snd b
{-# INLINE eitherOr #-}

----------------------------------------------------------------

-- | A variant of 'zip' which exhausts both lists, annotating which
-- list the elements came from. It will return zero or more @Both@,
-- followed by either zero or more @Fst@ or else zero or more @Snd@.
--
-- On GHC this is a \"good producer\" for list fusion.
zipOr :: [a] -> [b] -> [Or a b]
#ifdef __GLASGOW_HASKELL__
zipOr xs ys = build (\f z -> zipOrWithBy id f z xs ys)
#else
zipOr = zipOrWithBy id (:) []
#endif
{-# INLINE zipOr #-}


-- | A variant of 'zipOr' with a custom 'Or'-homomorphism.
--
-- On GHC this is a \"good producer\" for list fusion.
zipOrWith :: (Or a b -> c) -> [a] -> [b] -> [c]
#ifdef __GLASGOW_HASKELL__
zipOrWith k xs ys = build (\f z -> zipOrWithBy k f z xs ys)
#else
zipOrWith k = zipOrWithBy k (:) []
#endif
{-# INLINE zipOrWith #-}


-- | A variant of 'zipOr' with a custom list-homomorphism.
zipOrBy :: (Or a b -> c -> c) -> c -> [a] -> [b] -> c
zipOrBy = zipOrWithBy id
{-# INLINE zipOrBy #-}


-- | A variant of 'zipOr' with both a custom 'Or'-homomorphism and
-- a custom list-homomorphism. This is no more powerful than
-- 'zipOrBy', but it may be more convenient to separate the handling
-- of 'Or' from the handling of @(:)@.
zipOrWithBy
    :: (Or a b -> c)    -- ^ 'Or' homomorphism
    -> (c -> d -> d)    -- ^ list homomorphism, @(:)@ part
    -> d                -- ^ list homomorphism, @[]@ part
    -> [a] -> [b] -> d
zipOrWithBy k f z = go
    where
    go []     []     = z
    go []     (y:ys) = f (k (Snd    y)) (go [] ys)
    go (x:xs) []     = f (k (Fst  x  )) (go xs [])
    go (x:xs) (y:ys) = f (k (Both x y)) (go xs ys)

----------------------------------------------------------------
----------------------------------------------------------- fin.

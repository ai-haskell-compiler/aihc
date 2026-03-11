{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Arrow.Transformer.Error
-- Copyright   :  (c) Ross Paterson 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable (multi-parameter type classes)
--
-- An arrow transformer that adds error handling.
--
-- /TODO:/ the operations here are inconsistent with other arrow transformers.

module Control.Arrow.Transformer.Error(
    ErrorArrow(ErrorArrow),
    runError,
    ArrowAddError(..),
    ) where

import Control.Arrow.Internals
import Control.Arrow.Operations
import Control.Arrow.Transformer

import Control.Applicative
import Control.Arrow
import Control.Category
import Data.Monoid
#if (MIN_VERSION_base(4,9,0)) && !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif

import Prelude hiding (id,(.))

-- | An arrow that augments an existing arrow with possible errors.
-- The 'ArrowError' class contains methods for raising and handling
-- these errors.

newtype ErrorArrow ex a b c = ErrorArrow (a b (Either ex c))

rstrength :: (Either ex a, b) -> Either ex (a, b)
rstrength (Left ex, _) = Left ex
rstrength (Right a, b) = Right (a, b)

-- | Encapsulate an error-raising computation,
-- by completely handling any errors.
--
-- Typical usage in arrow notation:
--
-- >    proc p -> ...
-- >        body `runError` \ex -> handler

runError :: ArrowChoice a =>
    ErrorArrow ex a e b  -- ^ computation that may raise errors
    -> a (e,ex) b        -- ^ computation to handle errors
    -> a e b
runError (ErrorArrow f) h =
    arr id &&& f >>> arr strength >>> h ||| arr id
      where
        strength (x, Left y) = Left (x, y)
        strength (_, Right z) = Right z

-- transformer

instance ArrowChoice a => ArrowTransformer (ErrorArrow ex) a where
    lift f = ErrorArrow (f >>> arr Right)

-- liftings of standard classes

instance ArrowChoice a => Category (ErrorArrow ex a) where
    id = ErrorArrow (arr Right)
    ErrorArrow f . ErrorArrow g =
        ErrorArrow (arr (either Left id) . right f . g)

instance ArrowChoice a => Arrow (ErrorArrow ex a) where
    arr f = ErrorArrow (arr (Right . f))
    first (ErrorArrow f) = ErrorArrow (first f >>> arr rstrength)

instance ArrowChoice a => ArrowChoice (ErrorArrow ex a) where
    left (ErrorArrow f) = ErrorArrow (left f >>> arr assocsum)

assocsum :: Either (Either a b) c -> Either a (Either b c)
assocsum (Left (Left a)) = Left a
assocsum (Left (Right b)) = Right (Left b)
assocsum (Right c) = Right (Right c)

instance (ArrowChoice a, ArrowApply a) => ArrowApply (ErrorArrow ex a) where
    app = ErrorArrow (arr (\(ErrorArrow f, x) -> (f, x)) >>> app)

-- this instance has the right type, but it doesn't satisfy right
-- tightening, or sliding of non-strict functions.

instance (ArrowChoice a, ArrowLoop a) => ArrowLoop (ErrorArrow ex a) where
    loop (ErrorArrow f) = ErrorArrow (loop (f >>> arr dist))
      where
        dist x = (fstRight x, snd $ fromRight x)
        fstRight (Left x) = Left x
        fstRight (Right (x,_)) = Right x
        fromRight (Left _) = error "fromRight"
        fromRight (Right y) = y

-- Other instances

instance ArrowChoice a => Functor (ErrorArrow ex a b) where
    fmap f g = g >>> arr f

instance ArrowChoice a => Applicative (ErrorArrow ex a b) where
    pure x = arr (const x)
    f <*> g = f &&& g >>> arr (uncurry id)

instance (Monoid ex, ArrowChoice a) => Alternative (ErrorArrow ex a b) where
    empty = zeroArrow
    f <|> g = f <+> g

#if MIN_VERSION_base(4,9,0)
instance (Monoid ex, ArrowChoice a) => Semigroup (ErrorArrow ex a b c) where
    (<>) = (<+>)
#endif

instance (Monoid ex, ArrowChoice a) => Monoid (ErrorArrow ex a b c) where
    mempty = zeroArrow
#if !(MIN_VERSION_base(4,11,0))
    mappend = (<+>)
#endif

-- fresh instances

instance ArrowChoice a => ArrowError ex (ErrorArrow ex a) where
    raise = ErrorArrow (arr Left)
    handle (ErrorArrow f) (ErrorArrow h) =
        ErrorArrow (arr id &&& f >>> arr strength >>> h ||| arr Right)
      where
        strength (x, Left y) = Left (x, y)
        strength (_, Right z) = Right z
    tryInUnless (ErrorArrow f) (ErrorArrow s) (ErrorArrow h) =
        ErrorArrow (arr id &&& f >>> arr distr >>> h ||| s)
      where
        distr (b, Left ex) = Left (b, ex)
        distr (b, Right c) = Right (b, c)

instance ArrowChoice a => ArrowAddError ex (ErrorArrow ex a) a where
    liftError = lift
    elimError = runError

instance (Monoid ex, ArrowChoice a) => ArrowZero (ErrorArrow ex a) where
    zeroArrow = ErrorArrow (arr (const (Left mempty)))

instance (Monoid ex, ArrowChoice a) => ArrowPlus (ErrorArrow ex a) where
    f <+> g = handle f $ handle (arr fst >>> g) $
        ErrorArrow (arr (\((_,ex1), ex2) -> Left (ex1 `mappend` ex2)))

-- liftings of other arrow classes

-- specializations of general promotions

instance (ArrowReader r a, ArrowChoice a) =>
        ArrowReader r (ErrorArrow ex a) where
    readState = lift readState
    newReader (ErrorArrow f) = ErrorArrow (newReader f)

instance (ArrowState s a, ArrowChoice a) =>
        ArrowState s (ErrorArrow ex a) where
    fetch = lift fetch
    store = lift store

instance (ArrowWriter w a, ArrowChoice a) =>
        ArrowWriter w (ErrorArrow ex a) where
    write = lift write
    newWriter (ErrorArrow f) = ErrorArrow (newWriter f >>> arr rstrength)

-- promotions

instance (ArrowAddReader r a a', ArrowChoice a, ArrowChoice a') =>
        ArrowAddReader r (ErrorArrow ex a) (ErrorArrow ex a') where
    liftReader (ErrorArrow f) = ErrorArrow (liftReader f)
    elimReader (ErrorArrow f) = ErrorArrow (elimReader f)

instance (ArrowAddState s a a', ArrowChoice a, ArrowChoice a') =>
        ArrowAddState s (ErrorArrow ex a) (ErrorArrow ex a') where
    liftState (ErrorArrow f) = ErrorArrow (liftState f)
    elimState (ErrorArrow f) = ErrorArrow (elimState f >>> arr rstrength)

instance (ArrowAddWriter w a a', ArrowChoice a, ArrowChoice a') =>
        ArrowAddWriter w (ErrorArrow ex a) (ErrorArrow ex a') where
    liftWriter (ErrorArrow f) = ErrorArrow (liftWriter f)
    elimWriter (ErrorArrow f) = ErrorArrow (elimWriter f >>> arr rstrength)

{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-|
Module:      Language.Haskell.TH.Lift.Generics
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott

"GHC.Generics"-based approach to implementing `lift`.
-}
module Language.Haskell.TH.Lift.Generics (
    -- * "GHC.Generics"-based 'lift' implementations
    --
    -- $genericLiftFunctions
      genericLift
    , genericLiftTyped
    , genericLiftTypedTExp
    , genericLiftTypedCompat

    -- * 'Generic' classes
    --
    -- | You shouldn't need to use any of these
    -- classes directly; they are only exported for educational purposes.
    , GLift(..)
    , GLiftDatatype(..)
    , GLiftArgs(..)
    -- * 'Lift' reexport
    , Lift(..)
    ) where

import Control.Monad (liftM, (>=>))

import Data.Char (ord)
import Data.Word (Word8)

import GHC.Generics

import GHC.Base (unpackCString#)
import GHC.Exts (Char(..), Double(..), Float(..), Int(..), Word(..))

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Syntax.Compat

-- $genericLiftFunctions
--
-- These functions leverage "GHC.Generics" to automatically implement 'lift'
-- implementations. These serve as 'Generic'-based alternatives to @DeriveLift@.
-- Here is an example of how to use them:
--
-- @
-- &#123;-&#35; LANGUAGE DeriveGeneric &#35;-&#125;
-- module Foo where
--
-- import GHC.Generics
-- import Language.Haskell.Lift.Generics
--
-- data Foo = Foo Int Char String
--   deriving Generic
--
-- instance Lift Foo where
--   lift = genericLift
-- &#35;if MIN&#95;VERSION&#95;template_haskell(2,9,0)
--   liftTyped = genericLiftTypedCompat
-- &#35;endif
-- @
--
-- Now you can splice @Foo@ values directly into Haskell source code:
--
-- @
-- &#123;-&#35; LANGUAGE TemplateHaskell &#35;-&#125;
-- module Bar where
--
-- import Foo
-- import Language.Haskell.TH.Syntax
--
-- foo :: Foo
-- foo = $(lift (Foo 1 \'a\' \"baz\"))
-- @

-- | Produce a generic definition of 'lift'.
genericLift :: (Quote m, Generic a, GLift (Rep a)) => a -> m Exp
genericLift = glift . from

-- | Like 'genericLift', but returns a 'Code' instead of an 'Exp'.
genericLiftTyped :: (Quote m, Generic a, GLift (Rep a)) => a -> Code m a
genericLiftTyped = unsafeCodeCoerce . genericLift

-- | Like 'genericLift', but returns a 'TExp' instead of an 'Exp'.
genericLiftTypedTExp :: (Quote m, Generic a, GLift (Rep a)) => a -> m (TExp a)
genericLiftTypedTExp = unsafeTExpCoerceQuote . genericLift

-- | Lift 'genericLift', but returns:
--
-- * A 'Code' (if using @template-haskell-2.17.0.0@ or later), or
-- * A 'TExp' (if using an older version of @template-haskell@)
--
-- This function is ideal for implementing the 'liftTyped' method of 'Lift'
-- directly, as its type changed in @template-haskell-2.17.0.0@.
genericLiftTypedCompat :: (Quote m, Generic a, GLift (Rep a)) => a -> Splice m a
#if MIN_VERSION_template_haskell(2,17,0)
genericLiftTypedCompat = genericLiftTyped
#else
genericLiftTypedCompat = genericLiftTypedTExp
#endif

-- | Class of generic representation types which can be converted to Template
-- Haskell expressions.
class GLift f where
    glift :: Quote m
          => f a    -- ^ The generic value
          -> m Exp  -- ^ The resulting Template Haskell expression

instance (Datatype d, GLiftDatatype f) => GLift (D1 d f) where
    glift d@(M1 x) = gliftWith pName mName x
      where
        pName, mName :: String
        pName = packageName d
        mName = moduleName d

-- | Class of generic representation types which can be converted to Template
-- Haskell expressions, given a package and module name.
class GLiftDatatype f where
    gliftWith :: Quote m
              => String -- ^ The package name
              -> String -- ^ The module name
              -> f a    -- ^ The generic value
              -> m Exp  -- ^ The resulting Template Haskell expression

instance GLiftDatatype V1 where
    -- While many instances for void types produce the laziest possible result
    -- (here, something like pure undefined), we choose to be stricter. There
    -- seems little if any benefit to delaying exceptions in this context.
    gliftWith _ _ x = case x of {}

instance (Constructor c, GLiftArgs f) => GLiftDatatype (C1 c f) where
    gliftWith pName mName c@(M1 x) =
      gliftArgs x (ConE (mkNameG_d pName mName cName))
      where
        cName :: String
        cName = conName c

instance (GLiftDatatype f, GLiftDatatype g) => GLiftDatatype (f :+: g) where
    gliftWith pName mName (L1 l) = gliftWith pName mName l
    gliftWith pName mName (R1 r) = gliftWith pName mName r

-- | Class of generic representation types which can conceptually be converted
-- to a list of Template Haskell expressions (which represent a constructors'
-- arguments).
class GLiftArgs f where
    -- | @gliftArgs e f@ applies @f@ to the zero or more arguments represented
    -- by @e@.
    gliftArgs :: Quote m => f a -> Exp -> m Exp

instance GLiftArgs U1 where
    -- This pattern match must be strict, because
    -- lift undefined really shouldn't just happen
    -- to work for unit types.
    gliftArgs U1 = return

instance Lift c => GLiftArgs (K1 i c) where
    gliftArgs (K1 x) h = AppE h `liftM` liftQuote x

instance GLiftArgs f => GLiftArgs (S1 s f) where
    gliftArgs (M1 x) = gliftArgs x

instance (GLiftArgs f, GLiftArgs g) => GLiftArgs (f :*: g) where
    gliftArgs (f :*: g) = gliftArgs f >=> gliftArgs g

instance GLiftArgs UAddr where
    gliftArgs (UAddr a) h = return $ AppE h (LitE (StringPrimL (word8ify (unpackCString# a))))
      where
        word8ify :: String -> [Word8]
        word8ify = map (fromIntegral . ord)

instance GLiftArgs UChar where
    gliftArgs (UChar c) h = return $ AppE h (LitE (CharPrimL (C# c)))

instance GLiftArgs UDouble where
    gliftArgs (UDouble d) h = return $ AppE h (LitE (DoublePrimL (toRational (D# d))))

instance GLiftArgs UFloat where
    gliftArgs (UFloat f) h = return $ AppE h (LitE (floatPrimL (toRational (F# f))))

instance GLiftArgs UInt where
    gliftArgs (UInt i) h = return $ AppE h (LitE (IntPrimL (toInteger (I# i))))

instance GLiftArgs UWord where
    gliftArgs (UWord w) h = return $ AppE h (LitE (WordPrimL (toInteger (W# w))))

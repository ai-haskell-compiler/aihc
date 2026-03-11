{-|
Module      : Data.Compat
Description : Backwards Compatibility Schemes for Arbitrary Data
Copyright   : Travis Whitaker 2019-2024
License     : MIT
Maintainer  : pi.boy.travis@gmail.com
Stability   : Provisional
Portability : Portable

See <http://programmable.computer/compatible.html> for a full exposition and
worked examples.
-}

{-# LANGUAGE ConstraintKinds
           , FlexibleContexts
           , KindSignatures
           , RankNTypes
           , ScopedTypeVariables
           , TypeFamilies
           , TypeApplications
           , TypeOperators
           #-}

module Data.Compat where

import Control.Applicative

import Data.Constraint

import Data.Kind

import Data.Proxy

-- | A class for backwards-compatible data.
class Compat a where
    -- | The predecessor for this type, i.e. the type for the data schema
    --   directly preceeding 'a'.
    type Pred a             :: Type
    -- | Any additional constraints required to yield data values. Typically
    --   this will be a class that provides a parser.
    type CompatConstraint a :: Type -> Constraint
    -- | A type for wrapping migration results. It is most useful if this type
    --   has `Alternative` and `Monad` instances, enabling the use of
    --   `getCompatible`. `Maybe` is a good first choice.
    type CompatF a          :: Type -> Type
    -- | How to migrate from a value of the preceeding schema to the current
    --   schema.
    migrate  :: Pred a -> (CompatF a) a
    continue :: Proxy a
             -> Maybe (Dict ( Compat (Pred a)
                            , (CompatConstraint a) (Pred a)
                            , CompatConstraint a ~ CompatConstraint (Pred a)
                            , CompatF a ~ CompatF (Pred a)
                            )
                      )

-- | Recursively migrate a data value to the most recent schema, if possible.
getCompatible
    :: forall a.
       ( Compat a
       , (CompatConstraint a) a
       , Alternative (CompatF a)
       , Monad (CompatF a)
       )
    => (forall c. (Compat c, (CompatConstraint a) c) => (CompatF a) c)
    -> (CompatF a) a
getCompatible f =
    let f' = case continue (Proxy :: Proxy a) of
                Nothing   -> empty
                Just Dict -> getCompatible f >>= migrate
    in f <|> f'

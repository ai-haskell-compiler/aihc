{-# LANGUAGE FlexibleContexts #-}

module Data.Optics.Operators where

import Control.Monad.State (MonadState, modify)
import Optics.Core (A_Setter, Is, Optic', (%~))

{- |
Modify the target of the optic by adding a value.

@
data Person = Person { age :: 'Int' } deriving ('GHC.Generics.Generic')

f :: 'MonadState' Person m => m ()
f = #age += 1
@
-}
(+=) :: (Is k A_Setter, MonadState s m, Num a) => Optic' k is s a -> a -> m ()
o += a = modify $ o %~ (+ a)

infixr 4 +=
{-# INLINE (+=) #-}

{- |
Modify the target of the optic by subtracting a value.

@
data Person = Person { age :: 'Int' } deriving ('GHC.Generics.Generic')

f :: 'MonadState' Person m => m ()
f = #age -= 1
@
-}
(-=) :: (Is k A_Setter, MonadState s m, Num a) => Optic' k is s a -> a -> m ()
o -= a = modify $ o %~ subtract a

infixr 4 -=
{-# INLINE (-=) #-}

{- |
Modify the target of the optic by multiplying a value.

@
data Person = Person { age :: 'Int' } deriving ('GHC.Generics.Generic')

f :: 'MonadState' Person m => m ()
f = #age *= 1
@
-}
(*=) :: (Is k A_Setter, MonadState s m, Num a) => Optic' k is s a -> a -> m ()
o *= a = modify $ o %~ (* a)

infixr 4 *=
{-# INLINE (*=) #-}

{- |
Modify the target of the optic by dividing a value.

@
data Person = Person { age :: 'Int' } deriving ('GHC.Generics.Generic')

f :: 'MonadState' Person m => m ()
f = #age //= 1
@
-}
(//=) :: (Is k A_Setter, MonadState s m, Fractional a) => Optic' k is s a -> a -> m ()
o //= a = modify $ o %~ (/ a)
{-# INLINE (//=) #-}

infixr 4 //=

{- |
This module provides a simple abstract data type for
a piece of a data stucture that can be read from and written to.
In contrast to "Data.Accessor.Basic" it is intended for unqualified import.
-}
module Data.Accessor
    ( Accessor, accessor,
      setVal, (Accessor.^=), getVal, (Accessor.^.), (Accessor.^:),
      getA, putA, (=:), (State.%=), modA, (State.%:),
      (.>), (<.),
    )
where

import qualified Data.Accessor.Basic as Accessor
import qualified Data.Accessor.MonadStatePrivate as State
import Control.Monad.Trans.State (StateT, )

-- |An @Accessor r a@ is an object that encodes how to
-- get and put a subject of type @a@ out of/into an object
-- of type @s@.
--
-- In order for an instance of this data structure @a@ to be
-- an 'Accessor', it must obey the following laws:
--
-- > getVal a (setVal a x r) = x
-- > setVal a (getVal a r) r = r
type Accessor r a = Accessor.T r a


-- |Construct an 'Accessor' from a @get@ and a @set@ method.
--
accessor ::
      (r -> a)       {- ^ get method -}
   -> (a -> r -> r)  {- ^ set method -}
   -> Accessor r a
accessor = flip Accessor.fromSetGet

-- |Get a value from a record field that is specified by an Accessor
getVal ::
      Accessor r a   {- ^ record field -}
   -> r              {- ^ record -}
   -> a              {- ^ value of the field in the record -}
getVal = Accessor.get

-- |Set a value of a record field that is specified by an Accessor
setVal ::
      Accessor r a   {- ^ record field  @f@ -}
   -> a              {- ^ value @x@ to be set -}
   -> r              {- ^ original record -}
   -> r              {- ^ new record with field @f@ changed to @x@ -}
setVal = Accessor.set



infixl 9 .>

{- |
Accessor composition:
Combine an accessor with an accessor to a sub-field.
Speak \"stack\".
-}
(.>) :: Accessor a b -> Accessor b c -> Accessor a c
(.>) = (Accessor..>)

infixr 9 <.
{- |
Accessor composition the other direction.

> (<.) = flip (.>)

You may also use the @(.)@ operator from Category class.
-}
(<.) :: Accessor b c -> Accessor a b -> Accessor a c
(<.) = (Accessor.<.)



infix 1 =:

{-# DEPRECATED (=:) "use (Data.Accessor.Monad.Trans.State.%=) from data-accessor-transformers package" #-}
{- |
An \"assignment operator\" for state monads.

> (=:) = putA
-}
(=:) :: Monad m => Accessor r a -> a -> StateT r m ()
(=:) = putA


{-# DEPRECATED getA "Data.Accessor.Monad.Trans.State.get from data-accessor-transformers package" #-}
-- | A structural dereference function for state monads.
getA :: Monad m => Accessor r a -> StateT r m a
getA = State.get

{-# DEPRECATED putA "Data.Accessor.Monad.Trans.State.set from data-accessor-transformers package" #-}
-- | A structural assignment function for state monads.
putA :: Monad m => Accessor r a -> a -> StateT r m ()
putA = State.set

{-# DEPRECATED modA "Data.Accessor.Monad.Trans.State.modify from data-accessor-transformers package" #-}
-- | A structural modification function for state monads.
modA :: Monad m => Accessor r a -> (a -> a) -> StateT r m ()
modA = State.modify

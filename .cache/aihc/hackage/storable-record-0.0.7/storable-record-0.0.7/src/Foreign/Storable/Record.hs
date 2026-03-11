{- |
Here we show an example of how to
define a Storable instance with this module.

> import Foreign.Storable.Record as Store
> import Foreign.Storable (Storable (..), )
>
> import Control.Applicative (liftA2, )
>
> data Stereo a = Stereo {left, right :: a}
>
> store :: Storable a => Store.Dictionary (Stereo a)
> store =
>    Store.run $
>    liftA2 Stereo
>       (Store.element left)
>       (Store.element right)
>
> instance (Storable a) => Storable (Stereo a) where
>    sizeOf = Store.sizeOf store
>    alignment = Store.alignment store
>    peek = Store.peek store
>    poke = Store.poke store


The @Stereo@ constructor is exclusively used
for constructing the @peek@ function,
whereas the accessors in the @element@ calls
are used for assembling the @poke@ function.
It is required that the order of arguments of @Stereo@
matches the record accessors in the @element@ calls.
If you want that the stored data correctly and fully represents
your Haskell data, it must hold:

>   Stereo (left x) (right x) = x   .

Unfortunately this cannot be checked automatically.
However, mismatching types that are caused by swapped arguments
are detected by the type system.
Our system performs for you:
Size and alignment computation, poking and peeking.
Thus several inconsistency bugs can be prevented using this package,
like size mismatching the space required by @poke@ actions.
There is no more restriction,
thus smart constructors and accessors
and nested records work, too.
For nested records however,
I recommend individual Storable instances for the sub-records.

You see it would simplify class instantiation
if we could tell the class dictionary at once
instead of defining each method separately.

In this implementation we tail pad records
according to the overall required alignment
in conformance to the Linux/X86 ABI.
-}
module Foreign.Storable.Record (
   Dictionary, Access,
   element, run,

   alignment, sizeOf,
   peek, poke,
   ) where

import Control.Monad.Trans.Writer
          (Writer, writer, runWriter, )
import Control.Monad.Trans.State
          (State, modify, get, runState, )
import Control.Applicative (Applicative(pure, (<*>)), )
import Data.Functor.Compose (Compose(Compose), )
import Data.Monoid (Monoid(mempty, mappend), )
import Data.Semigroup (Semigroup((<>)), )

import Foreign.Storable.FixedArray (roundUp, )
import qualified Foreign.Storable as St

import Foreign.Ptr (Ptr, )
import Foreign.Storable (Storable, )


data Dictionary r =
   Dictionary {
      sizeOf_ :: Int,
      alignment_ :: Alignment,
      ptrBox :: Box r r
   }

newtype Access r a =
   Access
      (Compose (Writer Alignment)
        (Compose (State Int)
          (Box r))
        a)

instance Functor (Access r) where
   {-# INLINE fmap #-}
   fmap f (Access m) = Access (fmap f m)

instance Applicative (Access r) where
   {-# INLINE pure #-}
   {-# INLINE (<*>) #-}
   pure a = Access (pure a)
   Access f <*> Access x = Access (f <*> x)


{-
For a version with (Ptr r) factored out, see RecordReaderPtr.
That is slightly slower.
-}
data Box r a =
   Box {
      peek_ :: Ptr r -> IO a,
      poke_ :: Ptr r -> r -> IO ()
   }

instance Functor (Box r) where
   {-# INLINE fmap #-}
   fmap f (Box pe po) =
      Box (fmap f . pe) po

instance Applicative (Box r) where
   {-# INLINE pure #-}
   {-# INLINE (<*>) #-}
   pure a = Box (const $ pure a) (const $ const $ pure ())
   f <*> x =
      Box
         (\ptr -> peek_ f ptr <*> peek_ x ptr)
         (\ptr r -> poke_ f ptr r >> poke_ x ptr r)


newtype Alignment = Alignment {deconsAlignment :: Int}

instance Semigroup Alignment where
   {-# INLINE (<>) #-}
   Alignment x <> Alignment y = Alignment (lcm x y)

instance Monoid Alignment where
   {-# INLINE mempty #-}
   {-# INLINE mappend #-}
   mempty = Alignment 1
   mappend = (<>)


{-# INLINE element #-}
element :: Storable a => (r -> a) -> Access r a
element f =
   let align = St.alignment (f (error "Storable.Record.element.alignment: content touched"))
       size  = St.sizeOf (f (error "Storable.Record.element.size: content touched"))
   in  Access $
       Compose $ writer $ flip (,) (Alignment align) $
       Compose $ do
          modify (roundUp align)
          offset <- get
          modify (+size)
          return $ Box
             (\ptr -> St.peekByteOff ptr offset)
             (\ptr -> St.pokeByteOff ptr offset . f)

{-# INLINE run #-}
run :: Access r r -> Dictionary r
run (Access (Compose m)) =
   let (Compose s, align) = runWriter m
       (box, size) = runState s 0
   in  Dictionary (roundUp (deconsAlignment align) size) align box


{-# INLINE alignment #-}
alignment :: Dictionary r -> r -> Int
alignment dict _ =
   deconsAlignment $ alignment_ dict

{-# INLINE sizeOf #-}
sizeOf :: Dictionary r -> r -> Int
sizeOf dict _ =
   sizeOf_ dict

{-# INLINE peek #-}
peek :: Dictionary r -> Ptr r -> IO r
peek dict ptr =
   peek_ (ptrBox dict) ptr

{-# INLINE poke #-}
poke :: Dictionary r -> Ptr r -> r -> IO ()
poke dict ptr =
   poke_ (ptrBox dict) ptr

# c-struct

## C definition

foo.h

```
#ifndef _FOO_H
#define _FOO_H

typedef struct { int x; int y; } Foo;

#endif
```

foo.c

```
#include <stdlib.h>
#include <stdio.h>
#include "foo.h"

Foo *
foo_copy(Foo *src)
{
	Foo *p = malloc(sizeof(Foo));
	p -> x = src -> x;
	p -> y = src -> y;
	return p;
}

void
foo_free(Foo *p)
{
	free(p);
}

void
foo_print(Foo *f)
{
	printf("Foo: x = %d, y = %d\n", f -> x, f -> y);
}

void
foo_scale(Foo *f, int s)
{
	f -> x = f -> x * s;
	f -> y = f -> y * s;
}
```

## Immutable

### Foo

Foo.hsc

```
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foo where

import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import Foreign.C.Types (CInt(..))
import Foreign.C.Struct (struct)

#include "foo.h"

struct "Foo" #{size Foo} #{alignment Foo}
	[	("x", ''CInt, [| #{peek Foo, x} |], [| #{poke Foo, x} |]),
		("y", ''CInt, [| #{peek Foo, y} |], [| #{poke Foo, y} |]) ]
	[''Show, ''Read, ''Eq, ''Ord, ''Bounded, ''Storable]

fooPrint :: Foo -> IO ()
fooPrint (Foo_ f) = withForeignPtr f c_foo_print

foreign import ccall "foo_print" c_foo_print :: Ptr Foo -> IO ()
```

You get newtype Foo.

```
> Foo 123 456
Foo {fooX = 123, fooY = 456}
> it { fooY = 654}
Foo {fooX = 123, fooY = 654}
> f = it
> fooPrint f
Foo: x = 123, y = 654
> g = read "Foo {fooX = 456, fooY = 123}" :: Foo
> g
Foo {fooX = 456, fooY = 123}
> f < g
True
> minBound :: Foo
Foo {fooX = -2147483648, fooY = -2147483648}
```

### FooIx

FooIx.hsc

```
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FooIx where

import Foreign.Storable (Storable, peekByteOff, pokeByteOff)
import Foreign.C.Types (CInt(..))
import Foreign.C.Struct (struct)
import Data.Array (Ix(..))

#include "foo.h"

newtype CIntIx = CIntIx CInt
	deriving (Show, Eq, Ord, Enum, Num, Real, Integral, Storable)

struct "FooIx" #{size Foo} #{alignment Foo}
	[	("x", ''CIntIx, [| #{peek Foo, x} |], [| #{poke Foo, x} |]),
		("y", ''CIntIx, [| #{peek Foo, y} |], [| #{poke Foo, y} |]) ]
	[''Show, ''Eq, ''Ord, ''Ix]

instance Ix CIntIx where
	range (l, u) = [l .. u]
	index (l, _) i = fromIntegral $ i - l
	inRange (l, u) i = l <= i && i <= u
```

You get newtype FooIx.

```
> :module + Data.Array
> listArray (FooIx 3 5, FooIx 4 7) [5 ..]
array (FooIx {fooIxX = CIntIx 3, fooIxY = CIntIx 5},FooIx {fooIxX = CIntIx 4, fooIxY = CIntIx 7}) [(FooIx {...
> a = it
> a ! FooIx 4 6
9
```

## Mutable

If you want to change values of a struct, you should use `structPrim`.

FooPrim.hsc

```
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FooPrim where

import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import Foreign.C.Types (CInt(..))
import Foreign.C.Struct (struct, structPrim)
import Control.Monad.Primitive (PrimMonad(..), unsafeIOToPrim)

#include "foo.h"

struct "Foo" #{size Foo} #{alignment Foo}
	[	("x", ''CInt, [| #{peek Foo, x} |], [| #{poke Foo, x} |]),
		("y", ''CInt, [| #{peek Foo, y} |], [| #{poke Foo, y} |]) ]
	[''Show, ''Read, ''Eq, ''Ord, ''Bounded]

foreign import ccall "foo_copy" c_foo_copy :: Ptr Foo -> IO (Ptr Foo)
foreign import ccall "foo_free" c_foo_free :: Ptr Foo -> IO ()

structPrim "Foo" 'c_foo_copy 'c_foo_free [''Show]

fooScale :: PtimMonad m => FooPrim (PrimState m) -> CInt -> m ()
fooScale (FooPrim f) s = unsafeIOToPrim $ withForeignPtr f (`c_foo_scale` s)

foreign import ccall "foo_scale" c_foo_scale :: Ptr Foo -> CInt -> IO ()
```

You get `FooPrim`, `FooST`, `FooIO`, `fooFreeze`, `fooThaw` and `fooCopy`.

```
> :modu	+ Control.Monad.Primitive
> :type fooFreeze
fooFreeze :: PrimMonad m => FooPrim (PrimState m) -> m Foo
> :type fooThaw
fooThaw :: PrimMonad m => Foo -> m (FooPrim (PrimState m))
> :type FooCopy
fooCopy
  :: PrimMonad m =>
     FooPrim (PrimState m) -> m (FooPrim (PrimState m))
> Foo 123 456
Foo {fooX = 123, fooY = 456}
> fooThaw it
FooPrim 0x00000000002354a60
> fp = it
> fooScale fp 3
> fooFreeze fp
Foo {fooX = 369, fooY = 1368}
```

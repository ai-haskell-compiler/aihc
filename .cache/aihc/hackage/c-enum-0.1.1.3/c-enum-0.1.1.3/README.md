# c-enum

foo.h

```c
#ifndef _FOO_H
#define _FOO_H

typedef enum { FOO_ERROR = - 1, FOO_ZERO, FOO_ONE, FOO_TWO, FOO_THREE } Foo;

#endif
```

Foo.hsc

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foo where

import Foreign.C.Enum

#include "foo.h"

enum "Foo" ''#{type Foo} [''Show, ''Read, ''Eq] [
	("FooError", #{const FOO_ERROR}),
	("FooZero", #{const FOO_ZERO}),
	("FooOne", #{const FOO_ONE}),
	("FooTwo", #{const FOO_TWO}),
	("FooThree", #{const FOO_THREE}) ]
```

You get patterns FooError, ..., FooThree.
And `instance Show Foo` and `instance Read Foo` like the following.

```
> FooOne
FooOne
> Foo 1
FooOne
> Foo 5
Foo 5
> read "FooTwo" :: Foo
FooTwo
> read "Foo 3" :: Foo
FooThree
```

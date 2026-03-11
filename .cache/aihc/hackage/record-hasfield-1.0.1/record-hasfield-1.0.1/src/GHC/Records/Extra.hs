{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Extensions over a future version of "GHC.Records".
  Provides the function 'modifyField' plus the orphan instances:

* @HasField \'(x1,x2)@ for selecting first the @x1@ field, then the @x2@ field.
  Available for @()@ and tuples up to arity 5.

* @HasField \"_1\" (a,b) a@ for selecting the first compomnent of a pair,
  plus similarly for all fields up of all tuples up to arity 5.

  Using these functions together you get:

> modifyField @'("_1","_2") negate ((1,2),3,4,5) == ((1,-2),3,4,5)
-}
module GHC.Records.Extra (
    module GHC.Records.Compat,
    modifyField,
) where

import GHC.Records.Compat

-- | Modify a field in a record.
modifyField :: forall x r a. (HasField x r a) => r -> (a -> a) -> r
modifyField r f = gen $ f val
  where
    (gen, val) = hasField @x r

instance HasField '() a a where
    hasField r = (id, r)

instance (a1 ~ r2, HasField x1 r1 a1, HasField x2 r2 a2) => HasField '(x1, x2) r1 a2 where
    hasField r = (gen1 . gen2, val2)
      where
        (gen1, val1) = hasField @x1 r
        (gen2, val2) = hasField @x2 val1

instance
    (a1 ~ r2, a2 ~ r3, HasField x1 r1 a1, HasField x2 r2 a2, HasField x3 r3 a3) =>
    HasField '(x1, x2, x3) r1 a3
    where
    hasField = hasField @'(x1, '(x2, x3))

instance
    (a1 ~ r2, a2 ~ r3, a3 ~ r4, HasField x1 r1 a1, HasField x2 r2 a2, HasField x3 r3 a3, HasField x4 r4 a4) =>
    HasField '(x1, x2, x3, x4) r1 a4
    where
    hasField = hasField @'(x1, '(x2, x3, x4))

instance
    (a1 ~ r2, a2 ~ r3, a3 ~ r4, a4 ~ r5, HasField x1 r1 a1, HasField x2 r2 a2, HasField x3 r3 a3, HasField x4 r4 a4, HasField x5 r5 a5) =>
    HasField '(x1, x2, x3, x4, x5) r1 a5
    where
    hasField = hasField @'(x1, '(x2, x3, x4, x5))

instance HasField "_1" (a, b) a where
    hasField (a, b) = ((,b), a)
instance HasField "_2" (a, b) b where
    hasField (a, b) = ((a,), b)

instance HasField "_1" (a, b, c) a where
    hasField (a, b, c) = ((,b,c), a)
instance HasField "_2" (a, b, c) b where
    hasField (a, b, c) = ((a,,c), b)
instance HasField "_3" (a, b, c) c where
    hasField (a, b, c) = ((a,b,), c)

instance HasField "_1" (a, b, c, d) a where
    hasField (a, b, c, d) = ((,b,c,d), a)
instance HasField "_2" (a, b, c, d) b where
    hasField (a, b, c, d) = ((a,,c,d), b)
instance HasField "_3" (a, b, c, d) c where
    hasField (a, b, c, d) = ((a,b,,d), c)
instance HasField "_4" (a, b, c, d) d where
    hasField (a, b, c, d) = ((a,b,c,), d)

instance HasField "_1" (a, b, c, d, e) a where
    hasField (a, b, c, d, e) = ((,b,c,d,e), a)
instance HasField "_2" (a, b, c, d, e) b where
    hasField (a, b, c, d, e) = ((a,,c,d,e), b)
instance HasField "_3" (a, b, c, d, e) c where
    hasField (a, b, c, d, e) = ((a,b,,d,e), c)
instance HasField "_4" (a, b, c, d, e) d where
    hasField (a, b, c, d, e) = ((a,b,c,,e), d)
instance HasField "_5" (a, b, c, d, e) e where
    hasField (a, b, c, d, e) = ((a,b,c,d,), e)

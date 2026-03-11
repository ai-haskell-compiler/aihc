{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- | Module    :  IsomorphismClass.Extra
-- Copyright   :  (C) 2023 Alexey Tochin
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Alexey Tochin <Alexey.Tochin@gmail.com>
--
-- Extra instances for 'IsomorphicTo' typeclass from 'isomorphism-class' package.
module IsomorphismClass.Extra () where

#if MIN_VERSION_isomorphism_class(0,3,0)
#else
import Control.Category (id)
#endif
import Data.Void (Void, absurd)
import IsomorphismClass (IsomorphicTo, to)
import Prelude (Either (Left, Right), fst, snd)

#if MIN_VERSION_isomorphism_class(0,3,0)
#else
instance {-# INCOHERENT #-} IsomorphicTo a a where
  to = id
#endif

-- Type products

instance {-# INCOHERENT #-} IsomorphicTo a (a, ()) where
  to = fst

instance {-# INCOHERENT #-} IsomorphicTo (a, ()) a where
  to = (,())

instance {-# INCOHERENT #-} IsomorphicTo a ((), a) where
  to = snd

instance {-# INCOHERENT #-} IsomorphicTo ((), a) a where
  to = ((),)

-- | Type product commutativity
--
-- ==== __Examples of usage__
--
-- >>> import IsomorphismClass.Isomorphism (iso)
-- >>> (iso :: (->) (a, b) (b, a)) (1, "x")
-- ("x",1)
instance {-# INCOHERENT #-} IsomorphicTo (a, b) (b, a) where
  to (b, a) = (a, b)

instance {-# INCOHERENT #-} IsomorphicTo (a, (b, c)) ((a, b), c) where
  to ((a, b), c) = (a, (b, c))

instance {-# INCOHERENT #-} IsomorphicTo ((a, b), c) (a, (b, c)) where
  to (a, (b, c)) = ((a, b), c)

instance {-# INCOHERENT #-} IsomorphicTo ((a, b), (c, d)) ((a, c), (b, d)) where
  to ((a, c), (b, d)) = ((a, b), (c, d))

-- instance {-# INCOHERENT #-} IsomorphicTo (a, (b, (c, d))) (a, ((c, d), b)) where
--  to (a, ((c, d), b)) = (a, (b, (c, d)))
--
-- instance {-# INCOHERENT #-} IsomorphicTo (a, ((c, d), b)) (a, (b, (c, d))) where
--  to (a, (b, (c, d))) = (a, ((c, d), b))

-- Type sums

instance {-# INCOHERENT #-} IsomorphicTo a (Either a Void) where
  to (Left a) = a
  to (Right a) = absurd a

instance {-# INCOHERENT #-} IsomorphicTo (Either a Void) a where
  to = Left

-- | Type sum commutativity.
--
-- ==== __Examples of usage__
--
-- >>> import IsomorphismClass.Isomorphism (iso)
-- >>> (iso :: (->) (Either a b) (Either b a)) (Left 1)
-- Right 1
-- >>> (iso :: (->) (Either a b) (Either b a)) (Right "x")
-- Left "x"
instance {-# INCOHERENT #-} IsomorphicTo a (Either Void a) where
  to (Right a) = a
  to (Left a) = absurd a

instance {-# INCOHERENT #-} IsomorphicTo (Either Void a) a where
  to = Right

instance {-# INCOHERENT #-} IsomorphicTo (Either a b) (Either b a) where
  to (Left b) = Right b
  to (Right b) = Left b

instance {-# INCOHERENT #-} IsomorphicTo (Either a (Either b c)) (Either (Either a b) c) where
  to (Left (Left a)) = Left a
  to (Left (Right b)) = Right (Left b)
  to (Right c) = Right (Right c)

instance {-# INCOHERENT #-} IsomorphicTo (Either (Either a b) c) (Either a (Either b c)) where
  to (Left a) = Left (Left a)
  to (Right (Left b)) = Left (Right b)
  to (Right (Right c)) = Right c

instance {-# INCOHERENT #-} IsomorphicTo (Either (Either a b) (Either c d)) (Either (Either a c) (Either b d)) where
  to (Left (Left a)) = Left (Left a)
  to (Left (Right c)) = Right (Left c)
  to (Right (Left b)) = Left (Right b)
  to (Right (Right d)) = Right (Right d)

-- instance {-# INCOHERENT #-} IsomorphicTo (Either a (Either b (Either c d))) (Either a (Either (Either c d) b)) where
--  to (Left a) = Left a
--  to (Right (Left b)) = Right (Right b)
--  to (Right (Right (Left c))) =   Right
--
--
--  to (a, ((c, d), b)) = (a, (b, (c, d)))
--
-- instance {-# INCOHERENT #-} IsomorphicTo (Either a (Either (Either c d) b)) (Either a (Either b (Either c d))) where
--  to (a, (b, (c, d))) = (a, ((c, d), b))

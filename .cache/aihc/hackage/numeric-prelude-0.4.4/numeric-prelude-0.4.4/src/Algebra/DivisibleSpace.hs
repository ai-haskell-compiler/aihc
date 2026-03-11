{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Algebra.DivisibleSpace where

import qualified Algebra.VectorSpace as VectorSpace

-- Is this right?
infix 7 </>

{-|
DivisibleSpace is used for free one-dimensional vector spaces.  It
satisfies

>  (a </> b) *> b = a

Examples include dollars and kilometers.
-}
class (VectorSpace.C a b) => C a b where
    (</>) :: b -> b -> a


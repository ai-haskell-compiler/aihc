{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Module      : TypeLevel.Number.Classes
-- Copyright   : Alexey Khudyakov
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability   : unstable
-- Portability : unportable (GHC only)
--
-- This module contain interface type classes for operations with type
-- level numbers.
module TypeLevel.Number.Classes ( -- * Comparison of numbers
                                  Compare
                                , compareN
                                  -- ** Data labels for types comparison
                                , IsLesser
                                , IsEqual
                                , IsGreater
                                  -- ** Specialized type classes
                                  -- $comparing
                                , Lesser
                                , LesserEq
                                , Greater
                                , GreaterEq
                                  -- ** Special traits
                                , Positive
                                , NonZero
                                  -- * Arithmetic operations on numbers
                                , Next
                                , nextN
                                , Prev
                                , prevN
                                , Negate
                                , negateN
                                , Add
                                , addN
                                , Sub
                                , subN
                                , Mul
                                , mulN
                                , Div
                                , divN
                                  -- * Special classes
                                , Normalized
                                ) where

----------------------------------------------------------------
-- Comparison
----------------------------------------------------------------

-- | Type family for comparing two numbers. It's expected that for any
-- two valid 'n' and 'm' 'Compare n m' is equal to IsLess when 'n<m', IsEqual
-- when 'n=m' and IsGreater when 'n>m'.
type family Compare n m :: *

compareN :: n -> m -> Compare n m
compareN _ _ = undefined

data IsLesser
data IsEqual
data IsGreater

instance Show IsLesser  where show _  = "IsLesser"
instance Show IsEqual   where show _  = "IsEqual"
instance Show IsGreater where show _  = "IsGreater"

----------------------------------------------------------------

-- $comparing
-- These type classes are meant to be used in contexts to ensure
-- relations between numbers. For example:
-- 
-- > someFunction :: Lesser n m => Data n -> Data m -> Data n
-- > someFunction = ...
--
-- They have generic instances and every number which is instance of
-- Compare type family is instance of these type classes.
-- 
-- These instance could have problems. They weren't exensively tested.
-- Also error messages are really unhelpful.

-- | Numbers n and m are instances of this class if and only is n < m.
class Lesser n m

-- | Numbers n and m are instances of this class if and only is n > m.
class Greater n m

-- | Numbers n and m are instances of this class if and only is n <= m.
class LesserEq n m

-- | Numbers n and m are instances of this class if and only is n >= m.
class GreaterEq n m

-- a b c are instance of class only when a ~ b or a ~ c. Require ovelapping.
class    OneOfTwo a b c
instance OneOfTwo a a b
instance OneOfTwo a b a
instance OneOfTwo a a a

instance (Compare n m ~ IsLesser ) => Lesser n m
instance (Compare n m ~ IsGreater) => Greater n m
-- Instances for LessEq and GreaterEq are trickier.
instance (OneOfTwo (Compare n m) IsLesser  IsEqual) => LesserEq n m
instance (OneOfTwo (Compare n m) IsGreater IsEqual) => GreaterEq n m

-- | Non-zero number. For naturals it's same as positive
class NonZero n

-- | Positive number. 
class Positive n

----------------------------------------------------------------

-- | Next number.
type family Next n :: *

nextN :: n -> Next n
nextN _ = undefined

-- | Previous number
type family Prev n :: *

prevN :: n -> Prev n
prevN _ = undefined

-- | Negate number.
type family Negate n :: *

negateN :: n -> Negate n
negateN _ = undefined

----------------------------------------------------------------

-- | Sum of two numbers.
type family  Add n m :: *

addN :: n -> m -> Add n m
addN _ _ = undefined

-- | Difference of two numbers.
type family Sub n m :: *

subN :: n -> m -> Sub n m
subN _ _ = undefined

-- | Product of two numbers.
type family Mul n m :: *
       
mulN :: n -> m -> Mul n m
mulN _ _ = undefined

-- | Division of two numbers. 'n' and 'm' should be instances of this
-- class only if remainder of 'n/m' is zero.
type family Div n m :: *

divN :: n -> m -> Div n m
divN _ _ = undefined

----------------------------------------------------------------

-- | Usually numbers have non-unique representation. This type family
-- is canonical representation of number.
type family Normalized n :: *

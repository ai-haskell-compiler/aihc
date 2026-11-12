-- | References to values that are known at compile time.
--
-- aihc does not support the @StaticPointers@ extension. Thus, no
-- expression can make a 'StaticPtr'. The type exists for library code that
-- names it.
module GHC.StaticPtr
  ( StaticPtr,
    deRefStaticPtr,
  )
where

-- | A reference to a value that is known at compile time.
newtype StaticPtr a = StaticPtr a

-- | The value of a static reference.
deRefStaticPtr :: StaticPtr a -> a
deRefStaticPtr (StaticPtr value) = value

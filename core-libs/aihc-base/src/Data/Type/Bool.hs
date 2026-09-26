{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Type-level booleans.
module Data.Type.Bool
  ( If,
    type (&&),
    type (||),
    Not,
  )
where

import GHC.Types (Bool (..))

-- | Pick a branch by a type-level boolean.
type If :: Bool -> k -> k -> k
type family If cond tru fls where
  If 'True tru _ = tru
  If 'False _ fls = fls

-- | Type-level "and".
type (&&) :: Bool -> Bool -> Bool
type family a && b where
  'False && _ = 'False
  'True && a = a
  a && 'False = 'False
  a && 'True = a
  a && a = a

infixr 3 &&

-- | Type-level "or".
type (||) :: Bool -> Bool -> Bool
type family a || b where
  'False || a = a
  'True || _ = 'True
  a || 'False = a
  a || 'True = 'True
  a || a = a

infixr 2 ||

-- | Type-level "not".
type Not :: Bool -> Bool
type family Not a = res | res -> a where
  Not 'False = 'True
  Not 'True = 'False

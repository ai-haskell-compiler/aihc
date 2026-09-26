{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}

-- | A pointer to a constant C value. A @capi@ wrapper spells a
-- @ConstPtr a@ as @const T *@, where @T@ is the C spelling of @a@.
module Foreign.C.ConstPtr
  ( ConstPtr (..),
  )
where

import GHC.Base ((.))
import GHC.Classes (Eq (..), Ord (..))
import GHC.Ptr (Ptr)
import GHC.Show (Show (..), showParen, showString)

type role ConstPtr phantom

newtype {-# CTYPE "const void*" #-} ConstPtr a = ConstPtr {unConstPtr :: Ptr a}
  deriving newtype (Eq, Ord)

instance Show (ConstPtr a) where
  showsPrec d (ConstPtr p) = showParen (d > 10) (showString "ConstPtr " . showsPrec 11 p)

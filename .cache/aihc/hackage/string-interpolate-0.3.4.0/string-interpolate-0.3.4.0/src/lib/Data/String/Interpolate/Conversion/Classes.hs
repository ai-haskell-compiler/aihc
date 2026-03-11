{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Data.String.Interpolate.Conversion.Classes
  ( B(..)
  , IsCustomSink, InterpSink(..), Interpolatable(..), proxyWrapper
  )
where

import Data.Kind  ( Type )
import Data.Proxy

import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as LB
import qualified Data.ByteString.Lazy    as LB
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Builder  as LT

-- |
-- We wrap the builders in B so that we can add a phantom type parameter.
-- This gives the inner `interpolate's enough information to know where
-- they're going and pick an instance, forcing all the types into lockstep.
newtype B dst a = B { unB :: a }
  deriving (Eq, Show)

-- | Does this type require special behavior when something is interpolated /into/ it?
type family IsCustomSink dst where
  IsCustomSink T.Text = 'True
  IsCustomSink LT.Text = 'True
  IsCustomSink LT.Builder = 'True
  IsCustomSink B.ByteString = 'True
  IsCustomSink LB.ByteString = 'True
  IsCustomSink LB.Builder = 'True
  IsCustomSink _ = 'False


-- | Used to indicate to GHC that the value of all the proxy arguments is the same.
proxyWrapper :: forall final flag . (IsCustomSink final ~ flag) => (Proxy flag -> final) -> final
proxyWrapper k =
  let proxy = Proxy @(IsCustomSink final)
  in  k proxy

-- | Something that can be interpolated into.
class IsCustomSink dst ~ flag => InterpSink (flag :: Bool) dst where
  type Builder flag dst :: Type

  -- | Meant to be used only for verbatim parts of the interpolation.
  ofString :: Proxy flag -> String -> B dst (Builder flag dst)
  -- |
  -- `build' should be 'in-order'; that is, the left builder comes from
  -- a string on the left, and the right builder comes from a string on the right.
  build :: Proxy flag -> B dst (Builder flag dst) -> B dst (Builder flag dst) -> B dst (Builder flag dst)
  finalize :: Proxy flag -> B dst (Builder flag dst) -> dst

-- |
-- Represents that we can interpolate objects of type src into a an
-- interpolation string that returns type dst.
class InterpSink flag dst => Interpolatable (flag :: Bool) src dst where
  interpolate :: Proxy flag -> src -> B dst (Builder flag dst)

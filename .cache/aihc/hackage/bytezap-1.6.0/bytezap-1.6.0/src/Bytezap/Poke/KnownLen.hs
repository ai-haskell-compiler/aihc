-- | 'P.Poke's with type-level poke length.
module Bytezap.Poke.KnownLen where

import Bytezap.Poke qualified as P
import GHC.TypeNats
import Data.ByteString qualified as BS
import GHC.Exts
import Bytezap.Common.TypeNats ( natValInt )

import Raehik.Compat.Data.Primitive.Types

newtype PokeKnownLen (len :: Natural) s =
    PokeKnownLen { unPokeKnownLen :: P.Poke s }

mappend' :: PokeKnownLen n s -> PokeKnownLen m s -> PokeKnownLen (n+m) s
mappend' (PokeKnownLen l) (PokeKnownLen r) = PokeKnownLen (l <> r)

mempty' :: PokeKnownLen 0 s
mempty' = PokeKnownLen mempty

runPokeKnownLenBS
    :: forall n. KnownNat n
    => PokeKnownLen n RealWorld
    -> BS.ByteString
runPokeKnownLenBS (PokeKnownLen p) = P.unsafeRunPokeBS (natValInt @n) p

prim :: Prim' a => a -> PokeKnownLen (SizeOf a) s
prim = PokeKnownLen . P.prim

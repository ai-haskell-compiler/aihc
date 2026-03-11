{-# LANGUAGE UndecidableInstances #-}

module Generic.Type.CstrPath
  ( type GCstrPath
  , GCstrChoice(..)
  ) where

import GHC.Generics
import GHC.TypeLits
import Data.Type.Bool ( type If )
import Data.Type.Equality ( type (==) )
import Data.Kind ( type Type )

-- | Which direction to take at a ':+:' constructor choice.
data GCstrChoice = GoL1 -- ^ left  (the 'L1' constructor)
                 | GoR1 -- ^ right (the 'R1' constructor)

-- | Get the path to a named constructor in a generic type representation.
--
-- The D1 meta must already be stripped.
type GCstrPath :: Symbol -> (k -> Type) -> Either ErrorMessage [GCstrChoice]
type family GCstrPath name gf where
    -- handle the V1 case early so we don't have to keep checking later
    GCstrPath name V1 = Left (Text "type is empty (no constructors)")
    GCstrPath name gf = GCstrPath' name '[ '( '[], gf)]

type family GCstrPath' name zippers where
    GCstrPath' name ('(bcs, (l :+: r)) : zippers) =
        GCstrPath' name ('((GoL1 : bcs), l) : '((GoR1 : bcs), r) : zippers)
    GCstrPath' name ('(bcs, (C1 (MetaCons name' _ _) _)) : zippers) =
        If (name == name') (Right (Reverse bcs)) (GCstrPath' name zippers)
    GCstrPath' name '[] = Left (Text "no matching constructor")

-- | Reverse a type level list.
type Reverse as = Reverse' as '[]
type family Reverse' (as :: [k]) (acc :: [k]) :: [k] where
  Reverse' '[]      acc = acc
  Reverse' (a : as) acc = Reverse' as (a : acc)

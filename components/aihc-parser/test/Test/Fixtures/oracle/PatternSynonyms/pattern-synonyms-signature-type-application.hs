{- ORACLE_TEST pass -}
{-# LANGUAGE ExplicitForAll, KindSignatures, PatternSynonyms, PolyKinds, TypeApplications #-}
module PatternSynonymsSignatureTypeApplication where

import Data.Kind (Type)
import Data.Typeable (Typeable)
import Type.Reflection (TypeRep)

pattern TypeRep :: forall {k :: Type} (a :: k). () => Typeable @k a => TypeRep @k a

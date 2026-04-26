{- ORACLE_TEST pass -}
{-# LANGUAGE PatternSynonyms, TypeApplications #-}
module PatternSynonymsSignatureTypeApplication where

pattern TypeRep :: forall {k :: Type} (a :: k). () => Typeable @k a => TypeRep @k a

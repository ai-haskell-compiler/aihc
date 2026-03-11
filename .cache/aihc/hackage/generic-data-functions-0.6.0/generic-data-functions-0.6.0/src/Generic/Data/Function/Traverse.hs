{-# LANGUAGE AllowAmbiguousTypes #-}

{- | 'traverse' for generic data types.

TODO This is harder to conceptualize than generic 'foldMap'. No nice clean
explanation yet.

This function can provide generic support for simple parser-esque types.
-}

module Generic.Data.Function.Traverse
  ( GenericTraverse(..)
  , genericTraverseNonSum , GTraverseNonSum
  , genericTraverseSum,     GTraverseSum
  , genericTraverseSumRaw
  ) where

import GHC.Generics

import Generic.Data.Function.Traverse.NonSum
import Generic.Data.Function.Traverse.Sum
import Generic.Data.Function.Traverse.Constructor
import Generic.Data.MetaParse.Cstr
import GHC.TypeLits ( symbolVal' )

-- | Generic 'traverse' over a term of non-sum data type @f a@,
--   where @f@ is set by the @tag@ you pass.
genericTraverseNonSum
    :: forall {k} (tag :: k) a
    .  ( Generic a
       , Functor (GenericTraverseF tag)
       , GTraverseNonSum tag (Rep a)
    ) => GenericTraverseF tag a
genericTraverseNonSum = to <$> gTraverseNonSum @tag

genericTraverseSum
    :: forall tag sumtag a pt
    .  ( Generic a
       , Functor (GenericTraverseF tag)
       , GTraverseSum tag sumtag (Rep a)
    ) => ParseCstrTo sumtag pt
      -> (String -> GenericTraverseF tag pt)
      -> (forall x. String -> GenericTraverseF tag x)
      -> (pt -> pt -> Bool)
      -> GenericTraverseF tag a
genericTraverseSum parseCstr ptGet fNoMatch ptEq =
    to <$> gTraverseSum @tag @sumtag parseCstr ptGet fNoMatch ptEq

genericTraverseSumRaw
    :: forall tag a pt
    .  ( Generic a
       , Functor (GenericTraverseF tag)
       , GTraverseSum tag Raw (Rep a)
    ) => (String -> pt)
      -> (String -> GenericTraverseF tag pt)
      -> (forall x. String -> GenericTraverseF tag x)
      -> (pt -> pt -> Bool)
      -> GenericTraverseF tag a
genericTraverseSumRaw parseCstr ptGet fNoMatch ptEq = to <$>
    gTraverseSum @tag @Raw (\p -> parseCstr (symbolVal' p)) ptGet fNoMatch ptEq

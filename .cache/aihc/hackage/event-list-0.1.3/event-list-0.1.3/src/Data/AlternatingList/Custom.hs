{- |
Copyright   :  (c) Henning Thielemann 2007

Maintainer  :  haskell@henning-thielemann.de
Stability   :  stable
Portability :  Haskell 98

Lists of elements of alternating type.
This module iuses custom data types which depend mutually.
This looks nicer but it lacks high level optimizations.
(They could be added, though.)
-}
module Data.AlternatingList.Custom where

infixr 5 :>, :<

{- |
A list of elements of alternating types,
where the types of the beginning and the end element are independent,
namely @a@ at the beginning, @b@ at the end.

Example:
@1 :> \'a\' :< 2 :> \'b\' :< End@
-}
data Disparate a b =
    a :> Uniform a b
  | End

{- |
A list of elements of alternating types,
where the type of the beginning and the end element is equal,
namely @b@.

Example:
@1 :> \'a\' :< 2 :> \'b\' :< 3 :> End@
-}
data Uniform a b =
    b :< Disparate a b



mapDisparate ::
   (a0 -> a1) -> (b0 -> b1) ->
   (Disparate a0 b0 -> Disparate a1 b1)
mapDisparate f g =
   foldrDisparate ((:>) . f) ((:<) . g) End

mapUniform ::
   (a0 -> a1) -> (b0 -> b1) ->
   (Uniform a0 b0 -> Uniform a1 b1)
mapUniform f g =
   foldrUniform ((:>) . f) ((:<) . g) End



foldrDisparate ::
   (a -> c -> d) -> (b -> d -> c) ->
   d -> Disparate a b -> d
foldrDisparate f g start a0 =
   case a0 of
      End -> start
      a :> bas -> f a (foldrUniform f g start bas)

foldrUniform ::
   (a -> c -> d) -> (b -> d -> c) ->
   d -> Uniform a b -> c
foldrUniform f g start (b :< abas) =
   g b (foldrDisparate f g start abas)

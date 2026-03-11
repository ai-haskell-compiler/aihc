module Data.Function.Syntax
  ( module X
  , (*.)
  , (**.)
  , (***.)
  , (****.)
  , (*****.)
  , (******.)
  , (*******.)
  , (********.)
  ) where

import Data.Composition as X
import Data.Function.Contravariant.Syntax as X
import Data.Function.Apply as X
import Data.Function.Flip  as X
import Data.Function.Slip  as X
import Data.Function.Twist as X


(*.) :: (a -> c -> d) -> (b -> c) -> a -> b -> d
(*.) = flip (-.*)

infixl 8 *.

(**.) :: (a -> b -> d -> e) -> (c -> d) -> a -> b -> c -> e
(**.) = flip (-.**)

infixl 8 **.

(***.) :: (a -> b -> c -> e -> f) -> (d -> e) -> a -> b -> c -> d -> f
(***.) = flip (-.***)

infixl 8 ***.

(****.) :: (a -> b -> c -> d -> f -> g) -> (e -> f) -> a -> b -> c -> d -> e -> g
(****.) = flip (-.****)

infixl 8 ****.

(*****.) :: (a -> b -> c -> d -> e -> g -> h) -> (f -> g) -> a -> b -> c -> d -> e -> f -> h
(*****.) = flip (-.*****)

infixl 8 *****.

(******.) :: (a -> b -> c -> d -> e -> f -> h -> i) -> (g -> h) -> a -> b -> c -> d -> e -> f -> g -> i
(******.) = flip (-.******)

infixl 8 ******.

(*******.) :: (a -> b -> c -> d -> e -> f -> g -> i -> j) -> (h -> i) -> a -> b -> c -> d -> e -> f -> g -> h -> j
(*******.) = flip (-.*******)

infixl 8 *******.

(********.) :: (a -> b -> c -> d -> e -> f -> g -> h -> j -> k) -> (i -> j) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> k
(********.) = flip (-.********)

infixl 8 ********.

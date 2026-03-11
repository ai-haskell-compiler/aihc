module UniqueLogic.ST.TF.Rule (
   -- * Custom rules
   generic2,
   generic3,
   -- * Common rules
   equ, pair, max, add, mul, square, pow,
   ) where

import qualified UniqueLogic.ST.TF.ZeroFractional as ZeroFractional
import qualified UniqueLogic.ST.TF.System as Sys
import qualified UniqueLogic.ST.TF.MonadTrans as UMT
import qualified Data.Ref as Ref

import Control.Applicative (liftA2, )

import qualified Prelude as P
import Prelude hiding (max)


generic2 ::
   (UMT.C w, Ref.C s) =>
   (b -> a) -> (a -> b) ->
   Sys.Variable w s a -> Sys.Variable w s b -> Sys.T w s ()
generic2 f g x y =
   sequence_ $
   Sys.assignment2 f y x :
   Sys.assignment2 g x y :
   []

generic3 ::
   (UMT.C w, Ref.C s) =>
   (b -> c -> a) -> (c -> a -> b) -> (a -> b -> c) ->
   Sys.Variable w s a -> Sys.Variable w s b -> Sys.Variable w s c -> Sys.T w s ()
generic3 f g h x y z =
   sequence_ $
   Sys.assignment3 f y z x :
   Sys.assignment3 g z x y :
   Sys.assignment3 h x y z :
   []


equ ::
   (UMT.C w, Ref.C s) =>
   Sys.Variable w s a -> Sys.Variable w s a -> Sys.T w s ()
equ = generic2 id id

{- |
@max x y z@ means @max x y = z@.
-}
max ::
   (Ord a, UMT.C w, Ref.C s) =>
   Sys.Variable w s a -> Sys.Variable w s a -> Sys.Variable w s a -> Sys.T w s ()
max =
   Sys.assignment3 P.max

{- |
You might be tempted to use the 'pair' rule to collect parameters
for rules with more than three arguments.
This is generally not a good idea since this way you lose granularity.
For building rules with more than three arguments,
please build according assignments with 'Sys.arg' and 'Sys.runApply'
and bundle these assignments to rules.
This is the way, 'generic2' and 'generic3' work.
-}
pair ::
   (UMT.C w, Ref.C s) =>
   Sys.Variable w s a -> Sys.Variable w s b -> Sys.Variable w s (a,b) -> Sys.T w s ()
pair x y xy =
   Sys.assignment3 (,) x y xy >>
   Sys.assignment2 fst xy x >>
   Sys.assignment2 snd xy y

{- |
@add x y z@ means @x+y=z@.
-}
add :: (Num a, UMT.C w, Ref.C s) =>
   Sys.Variable w s a -> Sys.Variable w s a -> Sys.Variable w s a -> Sys.T w s ()
add = generic3 subtract (-) (+)

{- |
@mul x y z@ means @x*y=z@.
-}
mul :: (ZeroFractional.C a, UMT.C w, Ref.C s) =>
   Sys.Variable w s a -> Sys.Variable w s a -> Sys.Variable w s a -> Sys.T w s ()
mul x y z =
   sequence_ $
   Sys.assignment3 (*) x y z :
   Sys.runApplyMaybe (fmap ZeroFractional.multiply (Sys.arg x)) z :
   Sys.runApplyMaybe (fmap ZeroFractional.multiply (Sys.arg y)) z :
   Sys.runApplyMaybe (liftA2 ZeroFractional.divide (Sys.arg z) (Sys.arg y)) x :
   Sys.runApplyMaybe (liftA2 ZeroFractional.divide (Sys.arg z) (Sys.arg x)) y :
   []

_mul :: (Fractional a, UMT.C w, Ref.C s) =>
   Sys.Variable w s a -> Sys.Variable w s a -> Sys.Variable w s a -> Sys.T w s ()
_mul = generic3 (flip (/)) (/) (*)

{- |
@square x y@ means @x^2=y@.
-}
square :: (Floating a, UMT.C w, Ref.C s) =>
   Sys.Variable w s a -> Sys.Variable w s a -> Sys.T w s ()
square = generic2 sqrt (^(2::Int))

{- |
@pow x y z@ means @x**y=z@.
-}
pow :: (Floating a, UMT.C w, Ref.C s) =>
   Sys.Variable w s a -> Sys.Variable w s a -> Sys.Variable w s a -> Sys.T w s ()
pow = generic3 (\x y -> y ** recip x) (flip logBase) (**)

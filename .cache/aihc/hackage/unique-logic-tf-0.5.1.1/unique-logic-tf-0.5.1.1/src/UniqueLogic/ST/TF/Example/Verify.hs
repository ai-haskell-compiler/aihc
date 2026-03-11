{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
module UniqueLogic.ST.TF.Example.Verify
{-# WARNING "This module is intended for documentation purposes. Do not import it!" #-}
 where

import qualified UniqueLogic.ST.TF.Example.Term as Term
import qualified UniqueLogic.ST.TF.ZeroFractional as ZeroFractional
import qualified UniqueLogic.ST.TF.Expression as Expr
import qualified UniqueLogic.ST.TF.System as Sys
import qualified UniqueLogic.ST.TF.MonadTrans as UMT
import UniqueLogic.ST.TF.Expression ((=:=))

import qualified Control.Monad.Trans.Except as ME
import qualified Control.Monad.Trans.Writer as MW
import qualified Control.Monad.Trans.Class as MT
import Control.Monad.Trans.Writer (writer, )
import Control.Monad.Trans.Maybe (MaybeT, mapMaybeT, )
import Control.Monad.ST (runST, )
import Control.Monad (liftM, liftM2, ap, when, )
import Control.Applicative (Applicative, pure, (<*>), )

import qualified Data.Ref as Ref

import Prelude hiding (max, log)



data Assign = Assign Term.Name (TrackedNumber Rational)
   deriving (Show)

type Assigns = [Assign]

data TrackedNumber a = TrackedNumber Term.T a
   deriving (Show)

instance Functor TrackedNumber where
   fmap f (TrackedNumber term a) = TrackedNumber term $ f a


tn1 :: (Term.T -> Term.T) -> (a -> b) -> TrackedNumber a -> TrackedNumber b
tn1 f g (TrackedNumber xt xn) = TrackedNumber (f xt) (g xn)

tn2 :: (Term.T -> Term.T -> Term.T) -> (a -> b -> c) -> TrackedNumber a -> TrackedNumber b -> TrackedNumber c
tn2 f g (TrackedNumber xt xn) (TrackedNumber yt yn) =
   TrackedNumber (f xt yt) (g xn yn)

instance Num a => Num (TrackedNumber a) where
   fromInteger n = TrackedNumber (fromInteger n) (fromInteger n)
   (+) = tn2 (+) (+)
   (-) = tn2 (-) (-)
   (*) = tn2 (*) (*)
   abs = tn1 abs abs
   signum = tn1 signum signum

instance Fractional a => Fractional (TrackedNumber a) where
   fromRational n = TrackedNumber (fromRational n) (fromRational n)
   (/) = tn2 (/) (/)

instance (ZeroFractional.C a) => ZeroFractional.C (TrackedNumber a) where
   multiply (TrackedNumber xt x) =
      fmap (TrackedNumber xt) $ ZeroFractional.multiply x
   divide (TrackedNumber zt z) (TrackedNumber xt x) =
      fmap (TrackedNumber (zt/xt)) $
      ZeroFractional.divide z x


instance (Monad m) => Functor (Track m) where
   fmap = liftM

instance (Monad m) => Applicative (Track m) where
   pure = return
   (<*>) = ap

instance (Monad m) => Monad (Track m) where
   return = Track . UMT.point
   x >>= k  =  Track $ UMT.bind (runTrack x) (runTrack . k)


instance MT.MonadTrans Track where
   lift = Track . MT.lift . MT.lift

instance UMT.C Track where
   point = return
   bind = (>>=)

class ToTrackedNumber a where
   toTrackedNumber :: a -> TrackedNumber Rational

instance (Real a) => ToTrackedNumber (TrackedNumber a) where
   toTrackedNumber (TrackedNumber term a) =
      TrackedNumber term $ toRational a

instance (ToTrackedNumber tn) => Sys.Value Track tn where
   data ValueConstraint Track tn =
           (ToTrackedNumber tn) => VerifyConstraint
   valueConstraint _ _ = VerifyConstraint

instance Sys.C Track where
   update al av act =
      case Sys.valueConstraint al av of
         VerifyConstraint ->
            Sys.updateAndCheck
               (\old new ->
                  inconsistency Nothing (toTrackedNumber old) (toTrackedNumber new))
               al av act


newtype
   Track m a =
      Track {runTrack :: ME.ExceptT Exception (MW.WriterT Assigns m) a}

data
   Exception =
      Exception (Maybe Term.Name) (TrackedNumber Rational) (TrackedNumber Rational)
   deriving (Show)

type Variable s = Sys.Variable Track s (TrackedNumber Rational)

globalVariable :: (Ref.C s) => Term.Name -> s (Variable s)
globalVariable name =
   Sys.globalVariable
      (\al av -> Sys.updateAndCheck (inconsistency $ Just name) al av . logUpdate name)


match :: (Eq a) => TrackedNumber a -> TrackedNumber a -> Bool
match (TrackedNumber _ x) (TrackedNumber _ y)  =  x==y

inconsistency ::
   Monad m =>
   Maybe Term.Name ->
   TrackedNumber Rational ->
   TrackedNumber Rational ->
   UMT.Wrap Track m ()
inconsistency name old new =
   when (not $ match old new) $
   UMT.wrap $ Track $ ME.throwE $ Exception name old new

logUpdate ::
   (Real a, Ref.C s) =>
   Term.Name ->
   MaybeT s (TrackedNumber a) ->
   MaybeT (UMT.Wrap Track s) (TrackedNumber a)
logUpdate name act = do
   tn@(TrackedNumber _ x) <- mapMaybeT UMT.lift act
   MT.lift $ UMT.wrap $ Track $ MT.lift $
      writer (TrackedNumber (Term.Var name) x, [Assign name $ fmap toRational tn])


example ::
   (Either Exception
       (Maybe (TrackedNumber Rational),
        Maybe (TrackedNumber Rational)),
    Assigns)
example =
   runST (do
      xv <- globalVariable "x"
      yv <- globalVariable "y"
      MW.runWriterT $ ME.runExceptT $ runTrack $ do
         Sys.solve $ do
            let x = Expr.fromVariable xv
                y = Expr.fromVariable yv
            x*3 =:= y/2
            5 =:= 2+x
--            4 =:= 2+x
         MT.lift $ liftM2 (,)
            (Sys.query xv)
            (Sys.query yv))

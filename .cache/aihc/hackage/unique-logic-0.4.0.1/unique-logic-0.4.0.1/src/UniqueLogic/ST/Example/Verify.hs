module UniqueLogic.ST.Example.Verify
{-# DEPRECATED "This module is intended for documentation purposes. Do not import it!" #-}
 where

import qualified UniqueLogic.ST.Example.Term as Term
import qualified UniqueLogic.ST.Expression as Expr
import qualified UniqueLogic.ST.System as Sys
import qualified UniqueLogic.ST.Duplicate as Duplicate
import qualified UniqueLogic.ST.MonadTrans as UMT
import UniqueLogic.ST.Expression ((=:=))

import qualified Control.Monad.Exception.Synchronous as ME
import qualified Control.Monad.Trans.Writer as MW
import qualified Control.Monad.Trans.Class as MT
import Control.Monad.Trans.Writer (writer, )
import Control.Monad.Trans.Maybe (MaybeT, mapMaybeT, )
import Control.Monad.ST (ST, runST, )
import Control.Monad (liftM, liftM2, ap, )
import Control.Applicative (Applicative, pure, (<*>), )

import qualified Prelude as P
import Prelude hiding (max, log)



data Assign = Assign Term.Name Term.T
   deriving (Show)

type Assigns = [Assign]

data TrackedNumber a = TrackedNumber Term.T a
   deriving (Show)

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

instance Eq a => Duplicate.C (TrackedNumber a) where
   accept (TrackedNumber _ x) (TrackedNumber _ y)  =  x==y


instance (Monad m) => Functor (Track m) where
   fmap = liftM

instance (Monad m) => Applicative (Track m) where
   pure = Track . UMT.point
   (<*>) = ap

instance (Monad m) => Monad (Track m) where
   return = pure
   x >>= k  =  Track $ UMT.bind (runTrack x) (runTrack . k)


instance MT.MonadTrans Track where
   lift = Track . MT.lift . MT.lift

instance UMT.C Track where
   point = return
   bind = (>>=)

instance Sys.C Track where
   doUpdate =
      Sys.updateAndCheck $ \_ _ ->
         UMT.wrap $ Track $ ME.throwT AnonymousException


newtype
   Track m a =
      Track {runTrack :: ME.ExceptionalT Exception (MW.WriterT Assigns m) a}

data
   Exception =
        Exception Term.Name (TrackedNumber Rational) (TrackedNumber Rational)
      | AnonymousException
   deriving (Show)

type Variable s = Sys.Variable Track s (TrackedNumber Rational)

globalVariable :: Term.Name -> ST s (Variable s)
globalVariable name =
   Sys.globalVariable
      (\al av -> Sys.updateAndCheck (inconsistency name) al av . update name)

inconsistency ::
   Monad m =>
   Term.Name ->
   TrackedNumber Rational ->
   TrackedNumber Rational ->
   UMT.Wrap Track m ()
inconsistency name old new =
   UMT.wrap $ Track $ ME.throwT $ Exception name old new

update ::
   Term.Name ->
   MaybeT (ST s) (TrackedNumber a) ->
   MaybeT (UMT.Wrap Track (ST s)) (TrackedNumber a)
update name act = do
   (TrackedNumber t x) <- mapMaybeT UMT.lift act
   MT.lift $ UMT.wrap $ Track $ MT.lift $
      writer (TrackedNumber (Term.Var name) x, [Assign name t])


example ::
   (ME.Exceptional Exception
       (Maybe (TrackedNumber Rational),
        Maybe (TrackedNumber Rational)),
    Assigns)
example =
   runST (do
      xv <- globalVariable "x"
      yv <- globalVariable "y"
      MW.runWriterT $ ME.runExceptionalT $ runTrack $ do
         Sys.solve $ do
            let x = Expr.fromVariable xv
                y = Expr.fromVariable yv
            x*3 =:= y/2
            5 =:= 2+x
         MT.lift $ liftM2 (,)
            (Sys.query xv)
            (Sys.query yv))

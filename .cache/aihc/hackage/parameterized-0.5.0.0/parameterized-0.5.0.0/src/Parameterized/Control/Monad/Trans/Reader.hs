{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Parameterized.Control.Monad.Trans.Reader where

import Control.Applicative
import Control.Monad
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Zip
import Data.Diverse
import qualified GHC.Generics as G
import Parameterized.Control.Monad

-- | Given a Reader that accepts @Which a@, and another Reader that accepts @Which b@
-- make a reader that accepts @Which (AppendUnique a b)@ and runs both readers if possible,
-- where the types in @Which a@ and @Which b@ may overlap,
-- but with the compile time constraint that all the types in (AppendUnique a b) are distinct.
newtype OverlappingWhichReader m r a = OverlappingWhichReader
    { runOverlappingWhichReader :: ReaderT r m a
    } deriving ( G.Generic
               , Functor
               , Applicative
               , Monad
               , Alternative
               , MonadPlus
               , MonadZip
               , MonadFix
               , Fail.MonadFail
               , MonadIO
               )

type instance PUnary (OverlappingWhichReader m) r = OverlappingWhichReader m r

instance Applicative m => PPointed (OverlappingWhichReader m) (Which '[]) where
    ppure = OverlappingWhichReader . pure

instance Alternative m => PEmpty (OverlappingWhichReader m) (Which '[]) where
    pempty = OverlappingWhichReader $ empty

instance ( Alternative m
         , Reinterpret b c
         , Reinterpret a c
         , c ~ AppendUnique a b
         ) =>
         PAlternative (OverlappingWhichReader m) (Which a) (Which b) (Which c) where
    (OverlappingWhichReader (ReaderT f)) `pappend` (OverlappingWhichReader (ReaderT g)) =
        OverlappingWhichReader $ ReaderT $ \c -> case (reinterpret c, reinterpret c) of
            (Left _, Left _) -> empty
            (Left _, Right b) -> g b
            (Right a, Left _) -> f a
            (Right a, Right b) -> f a <|> g b

-------------------------------

-- | Given a Reader that accepts @Which a@, and another Reader that accepts @Which b@
-- make a reader that accepts @Which (Append a b)@ and only run one of the readers for the correct Which type,
-- with a compile-time contraint that the types in @Which a@ are distinct from the type in @Which b@
-- The advantage of 'DistinctWhichReader' over 'OverlappingWhichReader' is that 'pappend' doesn't
-- require the inner monad @m@ to be an 'Alternative'.
-- NB. 'PEmpty' still requires 'Alternative' but you don't need to be an instance of 'PEmpty'
-- (analogous to Semigroup)
newtype DistinctWhichReader m r a = DistinctWhichReader
    { runDistinctWhichReader :: ReaderT r m a
    } deriving ( G.Generic
               , Functor
               , Applicative
               , Monad
               , Alternative
               , MonadPlus
               , MonadZip
               , MonadFix
               , Fail.MonadFail
               , MonadIO
               )

type instance PUnary (DistinctWhichReader m) r = DistinctWhichReader m r

instance Applicative m => PPointed (DistinctWhichReader m) (Which '[]) where
    ppure = DistinctWhichReader . pure

instance Alternative m => PEmpty (DistinctWhichReader m) (Which '[]) where
    pempty = DistinctWhichReader $ empty

instance ( Reinterpret b c
         , Complement c b ~ a
         , Complement c a ~ b
         , c ~ Append a b
         ) =>
         PAlternative (DistinctWhichReader m) (Which a) (Which b) (Which c) where
    pappend (DistinctWhichReader (ReaderT f)) (DistinctWhichReader (ReaderT g)) =
        DistinctWhichReader . ReaderT $ \c -> case reinterpret c of
            Left a -> f a
            Right b -> g b

-------------------------------

-- | Given a Reader that accepts @Many a@, and another Reader that accepts @Many b@
-- make a reader that accepts @Many (AppendUnique a b)@
-- with the compile time constraint that all the types in (AppendUnique a b) are distinct.
newtype ManyReader m r a = ManyReader
    { runManyReader :: ReaderT r m a
    } deriving ( G.Generic
               , Functor
               , Applicative
               , Monad
               , Alternative
               , MonadPlus
               , MonadZip
               , MonadFix
               , Fail.MonadFail
               , MonadIO
               )

type instance PUnary (ManyReader m) r = ManyReader m r

instance Applicative m => PPointed (ManyReader m) (Many '[]) where
    ppure = ManyReader . pure

instance Alternative m => PEmpty (ManyReader m) (Many '[]) where
    pempty = ManyReader $ empty

instance ( Functor (ManyReader m (Many c))
         , Applicative m
         , Select a c
         , Select b c
         , c ~ AppendUnique a b
         ) =>
         PApplicative (ManyReader m) (Many a) (Many b) (Many c) where
    papply (ManyReader (ReaderT f)) (ManyReader (ReaderT g)) =
        ManyReader . ReaderT $ \c -> f (select c) <*> g (select c)

instance ( Alternative m
         , Select a c
         , Select b c
         , c ~ AppendUnique a b
         ) =>
         PAlternative (ManyReader m) (Many a) (Many b) (Many c) where
    pappend (ManyReader (ReaderT f)) (ManyReader (ReaderT g)) =
        ManyReader . ReaderT $ \c -> f (select c) <|> g (select c)

instance ( Functor (ManyReader m (Many c))
         , Monad m
         , Select a c
         , Select b c
         , c ~ AppendUnique a b
         ) =>
         PMonad (ManyReader m) (Many a) (Many b) (Many c) where
    pbind (ManyReader (ReaderT f)) k =
        ManyReader . ReaderT $ \c ->
            f (select c) >>= (k' (select c))
      where
        k' b a = let ManyReader (ReaderT g) = k a in g b

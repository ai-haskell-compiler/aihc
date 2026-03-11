{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

module Pipes.Fluid.Simultaneous
    ( Simultaneous(..)
    ) where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import qualified Pipes as P
import qualified Pipes.Prelude as PP

-- | The applicative instance of this combines multiple Producers synchronously
-- ie, yields a value only when both of the input producers yields a value.
-- Ends as soon as any of the input producer is ended.
newtype Simultaneous m a = Simultaneous
    { simultaneously :: P.Producer a m ()
    }

makeWrapped ''Simultaneous

instance Monad m => Functor (Simultaneous m) where
    fmap f (Simultaneous as) = Simultaneous $ as P.>-> PP.map f

instance Monad m => Applicative (Simultaneous m) where
    pure = Simultaneous . forever . P.yield
    Simultaneous xs <*> Simultaneous ys = Simultaneous $ go xs ys

go :: Monad m => P.Producer (t -> a) m r -> P.Producer t m r -> P.Proxy x' x () a m ()
go fs as = do
    rf <- lift $ P.next fs
    ra <- lift $ P.next as
    case (rf, ra) of
        (Left _, _) -> pure ()
        (_, Left _) -> pure ()
        (Right (f, fs'), Right (a, as')) -> do
            P.yield $ f a
            go fs' as'

instance Monad m => Monad (Simultaneous m) where
    Simultaneous as >>= f = Simultaneous $ do
        ra <- lift $ P.next as
        case ra of
            Left _ -> pure ()
            Right (a, as') -> do
                rb <- lift . P.next . simultaneously $ f a
                case rb of
                    Left _ -> pure ()
                    Right (b, _) -> do
                        P.yield b
                        simultaneously $ Simultaneous as' >>= f

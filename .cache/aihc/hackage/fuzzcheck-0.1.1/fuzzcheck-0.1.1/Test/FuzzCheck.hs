{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Please see the README at:
--
--   https://github.com/jwiegley/fuzzcheck/blob/master/README.md

module Test.FuzzCheck
       ( Fuzz(..)
       , FuzzException(..)
       , arg
       , gen
       , rand
       , branch
       , jumble
       , (?>)
       , fuzzCheck'
       , fuzzCheck
       ) where

import Control.Applicative
import Control.Exception.Lifted
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Functor.Product
import Data.List
import Data.Typeable
import Prelude hiding (ioError)
import Test.QuickCheck
import Test.QuickCheck.Gen (Gen(..))
import Test.QuickCheck.Random

newtype Fuzz a = Fuzz (Compose Gen (Product (Const [String]) Identity) a)
               deriving Functor

instance Applicative Fuzz where
    pure x = Fuzz (Compose (pure (Pair (Const ["<arg>"]) (Identity x))))
    Fuzz f <*> Fuzz x = Fuzz (f <*> x)

data FuzzException = FuzzException String deriving (Eq, Show, Typeable)
instance Exception FuzzException

wrap :: Show a => a -> Product (Const [String]) Identity a
wrap x = Pair (Const [show x]) (Identity x)

arg :: Show a => a -> Fuzz a
arg = Fuzz . Compose . pure . wrap

gen :: Show a => Gen a -> Fuzz a
gen (MkGen m) = Fuzz (Compose (MkGen g))
  where g r n = let x = m r n in wrap x

rand :: (Arbitrary a, Show a) => Fuzz a
rand = gen arbitrary

branch :: (MonadIO m, MonadBaseControl IO m) => [m a] -> m a
branch xs = do
    let len = length xs
    n <- "pick a random number"
             ?> return <$> gen (choose (0,len-1) :: Gen Int)
    xs !! n

jumble :: (MonadIO m, MonadBaseControl IO m) => [m a] -> m [a]
jumble xs = do
    let len = length xs
    xs' <- sequence xs
    foldM (\acc _x -> do
                n <- "pick a random number"
                         ?> return <$> gen (choose (1,len-1) :: Gen Int)
                let (y:ys, z:zs) = splitAt n acc
                return $ (z:ys) ++ (y:zs)) xs' xs'

infixr 1 ?>
(?>) :: (MonadIO m, MonadBaseControl IO m)
     => String -> Fuzz (m a) -> m a
lbl ?> Fuzz (Compose (MkGen g)) = do
    rnd <- liftIO newQCGen
    let Pair (Const args) (Identity x) = g rnd 100
    runFuzz args x
  where
    runFuzz :: (MonadIO m, MonadBaseControl IO m)
            => [String] -> m a -> m a
    runFuzz args m = m `catch` report
      where report e = throwIO (FuzzException $
                                lbl ++ " " ++ unwords (map show args)
                                    ++ ": " ++ show (e :: SomeException))

fuzzCheck' :: (MonadIO m, MonadBaseControl IO m)
           => m a -> Int -> m () -> m ()
fuzzCheck' f runs cleanup = replicateM_ runs f `finally` cleanup

fuzzCheck :: (MonadIO m, MonadBaseControl IO m)
          => m a -> m ()
fuzzCheck f = fuzzCheck' f 100 $
              liftIO $ putStrLn "+++ OK, passed 100 tests."

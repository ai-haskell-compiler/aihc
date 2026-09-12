module Continuation where

newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

callCC :: ((a -> ContT r m b) -> ContT r m a) -> ContT r m a
callCC f = ContT (\c -> runContT (f (\x -> ContT (\_ -> c x))) c)

newtype Constant e a = Constant e

newtype IdentityT m a = IdentityT (m a)

data Unit = Unit

type Alias = Constant Unit

newtype Identity a = Identity a

type Cont r = ContT r Identity

type Raw a = Constant Unit a

type Concrete = Raw Unit

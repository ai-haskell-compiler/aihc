{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Type.Flip where

newtype Flip t a b = Flip { unflip :: t b a } deriving Show

infixl 4 <$%>, <*%>

ffmap, (<$%>) :: Functor (Flip t c) => (a -> b) -> t a c -> t b c
ffmap f = unflip . fmap f . Flip
(<$%>) = ffmap

fpure :: Applicative (Flip t b) => a -> t a b
fpure = unflip . pure

(<*%>) :: Applicative (Flip t c) => t (a -> b) c -> t a c -> t b c
mf <*%> mx = unflip $ Flip mf <*> Flip mx

infixl 1 >>=%

(>>=%) :: Monad (Flip t c) => t a c -> (a -> t b c) -> t b c
m >>=% f = unflip $ Flip m >>= Flip . f

infixr 1 =<<%

(=<<%) :: Monad (Flip t c) => (a -> t b c) -> t a c -> t b c
(=<<%) = flip (>>=%)

ftraverse :: (Applicative (Flip s c), Traversable t) => (a -> s b c) -> t a -> s (t b) c
ftraverse f t = unflip $ traverse (Flip . f) t

fsequence :: (Applicative (Flip s c), Traversable t) => t (s a c) -> s (t a) c
fsequence tf = unflip . sequenceA $ Flip <$> tf

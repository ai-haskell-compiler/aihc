{-# LANGUAGE DeriveFunctor #-}

-- | Some useful Monads
module System.FilePattern.Monads(
    Next, runNext, getNext
    ) where

import Control.Applicative
import Prelude

-- | Next is a monad which has a series of elements, and can pull off the next one
newtype Next e a = Next ([e] -> Maybe ([e], a))
    deriving Functor

instance Applicative (Next e) where
    pure a = Next $ \es -> Just (es, a)
    Next f <*> Next x = Next $ \es -> do
        (es, f) <- f es
        (es, x) <- x es
        Just (es, f x)

getNext :: Next e e
getNext = Next $ \x -> case x of
    e:es -> Just (es, e)
    _ -> Nothing

runNext :: [e] -> Next e a -> Maybe ([e], a)
runNext ps (Next f) = f ps

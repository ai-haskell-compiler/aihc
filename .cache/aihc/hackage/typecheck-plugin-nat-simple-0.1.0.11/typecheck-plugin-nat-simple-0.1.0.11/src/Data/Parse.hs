{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Parse (Parse, parse, unparse, (>>!)) where

import Control.Applicative (empty)
import Control.Monad ((>=>))
import Control.Monad.StateT (StateT(..))

type Parse s = StateT s Maybe

parse :: (s -> Maybe (a, s)) -> Parse s a
parse = StateT

unparse :: Parse s a -> s -> Maybe (a, s)
unparse = runStateT

(>>!) :: Parse s a -> Parse s b -> Parse s a
p >>! nla = parse $ unparse p >=> \r@(_, s) ->
	maybe (pure r) (const empty) $ nla `unparse` s

module Reactive.Banana.Bunch.Private where

import qualified Reactive.Banana.Combinators as RB

import Control.Applicative ((<$>))

import qualified Data.NonEmpty as NonEmpty


newtype Event a = Event (RB.Event (NonEmpty.T [] a))

instance Functor Event where
   fmap f (Event xs) = Event $ fmap f <$> xs


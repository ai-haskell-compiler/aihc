{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Spreadsheet.CharSource where

import Control.Monad.Trans.State (StateT(StateT), gets, runStateT, mapStateT, )
import Control.Applicative (Applicative, )
import Data.Functor.Identity (Identity(Identity), runIdentity, )

import Data.List.HT (viewL, )
import Data.Tuple.HT (forcePair, )

import qualified Prelude as P
import Prelude hiding (String)


class (Monad (m Maybe), Monad (m Identity)) => C m where
   get   :: m Maybe Char
   isEnd :: m Identity Bool
   stop  :: m Maybe a
   fallible :: m Identity a -> m Maybe a
   {- |
   Try to run a parser.
   If it succeeds, return (Just value) and advance input position.
   If it fails, return Nothing and keep the input position.
   -}
   try  :: m Maybe a -> m Identity (Maybe a)


newtype String fail a = String {runString :: StateT P.String fail a}
   deriving (Functor, Applicative, Monad)


instance C String where
   get   = String $ StateT viewL
   isEnd = String $ gets null
   stop  = String $ StateT $ const Nothing
   fallible x = String $ mapStateT (Just . runIdentity) $ runString x
   try x = String $ StateT $ \s0 ->
      Identity $ forcePair $
      case runStateT (runString x) s0 of
         Nothing -> (Nothing, s0)
         Just (a,s1) -> (Just a, s1)

module Text.HTML.Tagchup.Parser.Status where

import qualified Text.HTML.Tagchup.Parser.Stream as Stream
import qualified Text.XML.Basic.Position as Position

import Control.Monad.Trans.State (StateT(..), )


data T stream =
   Cons {
      sourcePos :: Position.T,
      source    :: stream
   }
   deriving Show


instance Stream.C input => Stream.C (T input) where
   getChar =
      StateT $ \ (Cons pos str) ->
         do (c,cs) <- runStateT Stream.getChar str
            return (c, Cons (Position.updateOnChar c pos) cs)

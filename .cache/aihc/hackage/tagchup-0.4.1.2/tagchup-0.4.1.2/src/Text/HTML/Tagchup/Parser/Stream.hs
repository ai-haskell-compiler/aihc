module Text.HTML.Tagchup.Parser.Stream where

import Control.Monad.Trans.State (StateT(StateT), gets, put, )
import Control.Monad (guard, mzero, )
import qualified Data.List.HT as L

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL

import qualified Prelude as P
import Prelude hiding (Char, getChar, )


class C stream where
   getChar :: StateT stream Maybe P.Char


class Char char where
   toChar :: char -> P.Char

instance Char P.Char where
   toChar = id


instance Char char => C [char] where
   getChar = fmap toChar $ StateT L.viewL

instance C BS.ByteString where
   getChar = StateT BS.uncons

instance C BL.ByteString where
   getChar = StateT BL.uncons


data PointerStrict =
   PointerStrict {psSource :: !BS.ByteString, psIndex :: !Int}

pointerFromByteStringStrict :: BS.ByteString -> PointerStrict
pointerFromByteStringStrict str =
   PointerStrict str 0

instance C PointerStrict where
   getChar =
      do s <- gets psSource
         i <- gets psIndex
         guard (i < BS.length s)
         put $ PointerStrict s (i+1)
         return (BS.index s i)



data PointerLazy =
   PointerLazy {plSource :: ![BS.ByteString], plIndex :: !Int}

pointerFromByteStringLazy :: BL.ByteString -> PointerLazy
pointerFromByteStringLazy str =
   PointerLazy (BL.toChunks str) 0

instance C PointerLazy where
   getChar =
      do s <- gets plSource
         i <- gets plIndex
         case s of
            [] -> mzero
            (c:cs) ->
               if i < BS.length c
                 then put (PointerLazy s (i+1)) >> return (BS.index c i)
                 else put (PointerLazy cs (i - BS.length c)) >> getChar

{- |
Reading records from streams

This is still only for demonstration and might be of not much use
and you should not rely on the interface.
-}
module Data.Accessor.BinaryRead (
   Stream,
   C(any),
   ByteSource(readWord8),
   ByteStream(getWord8),
   ByteCompatible(toByte),
   Parser(Parser, runParser),
   field,
   record,
   ) where

import qualified Data.Accessor.Basic as Accessor
import Data.Accessor.ByteSource
         (ByteSource(..), ByteStream(..), ByteCompatible(..))

import qualified Control.Monad.Trans.State as State
import Control.Monad (liftM, )
import Data.Word (Word8, )
import Data.Char (chr, )

import Prelude hiding (any)


type Stream = [Word8]

class C a where
   any :: ByteSource source => source a

instance C Word8 where
   any = readWord8

instance C Char where
   any =
      liftM (chr . fromIntegral) readWord8

instance C Int where
   any =
      do c0 <- readWord8
         c1 <- readWord8
         c2 <- readWord8
         c3 <- readWord8
         return
            (foldl1 (\acc d -> acc*256+d)
               (map fromIntegral [c0,c1,c2,c3]))


newtype Parser s r = Parser {runParser :: (r, s) -> Maybe (r, s)}


field :: (ByteStream s, C a) =>
   Accessor.T r a -> Parser s r
field f =
   Parser $
      uncurry (\r -> State.runStateT $
         fmap (\x -> Accessor.set f x r) any)

record :: [Parser s r] -> Parser s r
record ps =
   Parser $ flip (foldl (>>=)) (map runParser ps) . Just

-- TODO: writer

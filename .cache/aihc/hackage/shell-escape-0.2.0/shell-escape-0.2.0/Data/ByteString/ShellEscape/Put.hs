
module Data.ByteString.ShellEscape.Put
  ( module Data.Binary.Put
  , putChar
  , putString
  , runPut'
  ) where

import Prelude hiding (putChar)
import Data.Binary.Put
import Data.Char
import Data.ByteString.Lazy
import Data.ByteString
import Data.ByteString.Char8

putChar                     ::  Char -> Put
putChar                      =  putWord8 . toEnum . fromEnum

putString                   ::  String -> Put
putString                    =  putByteString . Data.ByteString.Char8.pack

runPut'                     ::  Put -> Data.ByteString.ByteString
runPut' = Data.ByteString.concat . Data.ByteString.Lazy.toChunks . runPut 


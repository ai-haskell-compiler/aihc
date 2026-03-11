module Main where

import Control.Monad (when)
import qualified Foreign.Marshal.Alloc as Alloc
import System.Environment (getArgs)
import qualified Foreign.C.String as C
import qualified Data.ByteString.Char8 as BS8
import qualified Foreign.C.Types as C
import Data.Time.Clock.POSIX
import Data.Function (fix)
import Foreign.Storable (peek)

import LibOath

-- this is basically just copying over the main function from oathtool.c
main :: IO ()
main = do
    (input : _) <- getArgs

    either print BS8.putStrLn =<< generateTOTPFromSecret (BS8.pack input)

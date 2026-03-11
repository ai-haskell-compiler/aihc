{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module System.TargetEndian (
	-- * To Select According To Endian
	endian ) where

import Language.Haskell.TH (ExpQ, runIO)
import Foreign.Ptr (castPtr)
import Foreign.Marshal (alloca, peekArray)
import Foreign.Storable (poke)
import Data.Word (Word8, Word32)
import System.Environment (lookupEnv)

---------------------------------------------------------------------------

endian :: ExpQ -> ExpQ -> ExpQ
endian el eb = runIO targetEndian >>= \case
	Right LittleEndian -> el; Right BigEndian -> eb
	Right UnknownEndian -> error "Unknown endian"; Left emsg -> error emsg

data Endian = LittleEndian | BigEndian | UnknownEndian deriving Show

targetEndian :: IO (Either String Endian)
targetEndian = lookupEnv "GHC_TARGET_ENDIAN" >>= \case
	Just "little-endian" -> pure $ Right LittleEndian
	Just "big-endian" -> pure $ Right BigEndian
	Just edn -> pure . Left $ "no such endian: " ++ edn ++ "\n" ++
		"\tGHC_TARGET_ENDIAN: little-endian or big-endian"
	Nothing -> Right <$> checkEndian

checkEndian :: IO Endian
checkEndian = (<$> alloca \p -> poke p word32 >> peekArray 4 (castPtr p)) \case
	[4 :: Word8, 3, 2, 1] -> LittleEndian; [1, 2, 3, 4] -> BigEndian
	_ -> UnknownEndian
	where word32 :: Word32; word32 = 0x01020304

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.List.Range
import System.IO

import qualified Data.ByteString.Char8 as BSC

type Password = RangeL 8 127 Char

getRangedPassword :: Unfoldr 0 n m => IO (Maybe (RangeL n m Char))
getRangedPassword = do
	e <- hGetEcho stdin
	hSetEcho stdin False
	unfoldrMRangeMaybe ((/= '\n') <$> hLookAhead stdin) getChar
		<* hSetEcho stdin e

passwordToByteString :: Password -> BSC.ByteString
passwordToByteString = foldr BSC.cons ""

main :: IO ()
main = do
	p <- getRangedPassword
	print p
	maybe (error "bad password length") BSC.putStrLn $ passwordToByteString <$> p

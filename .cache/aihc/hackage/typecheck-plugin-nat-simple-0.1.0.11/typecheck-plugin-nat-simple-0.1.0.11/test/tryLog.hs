{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import GHC.Utils.Outputable
import Control.Monad.Try hiding (Message)
import Data.String
import Data.Log

main :: IO ()
main = do
	let	(r, l) = runTry do
			n <- 8 `devide` 3 :: Try (Log String Int) (Log String Int) Int
			(15 `devide` 0) `catch` \e -> tell e >> pure (n + 0)
	putStr $ message l
--	putStrLn . showSDocUnsafe $ ppr l
	print r

devide :: forall s . (IsString s, Message s) => Int -> Int -> Try (Log s Int) (Log s Int) Int
m `devide` 0 = throw $ logVar m .+. " is devided by " .+. logVar 0
m `devide` n = do
	tell (logVar m .+. " is devided by " .+. logVar n :: Log s Int)
	pure $ m `div` n

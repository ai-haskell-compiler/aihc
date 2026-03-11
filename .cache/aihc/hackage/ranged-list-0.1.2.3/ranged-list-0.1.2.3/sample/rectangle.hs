{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.List.Length

data Rect = Rect {
	left :: Double, top :: Double,
	width :: Double, height :: Double } deriving Show

makeRect :: LengthL 4 Double -> Rect
makeRect (l :. t :. w :. h :. NilL) = Rect l t w h

main :: IO ()
main = print $ map makeRect . fst $ chunksL [3, 5, 15, 2, 8, 4, 1, 9, 3, 5]

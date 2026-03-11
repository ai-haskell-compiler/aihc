{-# LANGUAGE Safe #-}
module Main (main) where

import qualified Data.ByteString.Char8 as B
import qualified Data.HashSet          as S

main :: IO ()
main = uniqDeep

uniqDeep :: IO ()
uniqDeep = B.getContents >>= B.putStrLn . B.unlines . deleteDuplicate . B.lines

deleteDuplicate :: [B.ByteString] -> [B.ByteString]
deleteDuplicate = check S.empty

-- | リストの遅延評価を活かして欲しいので、fold系を安易に使いたくないので、愚直に再帰します。
check :: S.HashSet B.ByteString -> [B.ByteString] -> [B.ByteString]
check _ [] = []
check already (l : ls)
  | l `S.member` already = check already ls
  | otherwise            = l : check (S.insert l already) ls

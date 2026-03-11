module Reactive.Banana.MIDI.DeBruijn where

import qualified Reactive.Banana.MIDI.Trie as Trie

import qualified Data.List.Match as Match
import qualified Data.List.HT as ListHT
import qualified Data.List as List
import Data.Maybe.HT (toMaybe, )

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Data.Bits as Bits
import Data.Bits ((.&.), )

import Control.Monad (guard, replicateM, )

import Prelude hiding (all, )


{- |
@'lexLeast' n k@ is a sequence with length n^k
where @cycle ('lexLeast' n k)@ contains all n-ary numbers with k digits as infixes.
The function computes the lexicographically smallest of such sequences.
-}
lexLeast :: Int -> Int -> [Int]
lexLeast n k =
   concat $
   filter ((0==) . mod k . length) $
   takeWhile (not . null) $
   iterate (nextLyndonWord n k) [0]

nextLyndonWord :: Int -> Int -> [Int] -> [Int]
nextLyndonWord n k =
   foldr
      (\x xs ->
         if null xs
           then (if x<n-1 then [x+1] else [])
           else x:xs) [] .
   take k . cycle


{- |
All Bruijn sequences with a certain alphabet and a certain length of infixes.
-}
all :: Int -> Int -> [[Int]]
all n k =
   let start = replicate k 0
       go _ str 0 = do
          guard $ str==start
          return []
       go set str c = do
          d <- [0 .. n-1]
          let newStr = tail str ++ [d]
          guard $ Set.notMember newStr set
          rest <- go (Set.insert newStr set) newStr (c-1)
          return $ d:rest
   in  map (ListHT.rotate (-k)) $
       go Set.empty start (n^k)

allMap :: Int -> Int -> [[Int]]
allMap n k =
   let start = replicate k 0
       delete d =
          Map.update (\set ->
             let newSet = Set.delete d set
             in  toMaybe (not $ Set.null newSet) newSet)
       go [] _ = error "infixes must have positive length"
       go (_:str) todo =
          case Map.lookup str todo of
             Nothing -> do
                guard $ Map.null todo
                return []
             Just set -> do
                d <- Set.toList set
                rest <- go (str ++ [d]) $ delete d str todo
                return $ d:rest
   in  map (take (n^k) . (start ++)) $
       go start $
       delete 0 (tail start) $
       Map.fromAscList $
       map (flip (,) $ Set.fromList [0 .. n-1]) $
       replicateM (k-1) [0 .. n-1]


allTrie :: Int -> Int -> [[Int]]
allTrie n k =
   let start = replicate k 0
       go [] _ = error "infixes must have positive length"
       go (_:str) todo =
          case Trie.lookup str todo of
             Nothing -> do
                guard $ Trie.null todo
                return []
             Just set -> do
                d <- set
                rest <- go (str ++ [d]) $ Trie.delete d str todo
                return $ d:rest
   in  map (take (n^k) . (start ++)) $
       go start $
       Trie.delete 0 (tail start) $
       Trie.full [0 .. n-1] [0 .. n-1] (k-1)


allBits :: Int -> Int -> [[Int]]
allBits n k =
   let go code todo =
          let shiftedCode = mod (code*n) (n^k)
          in  case Bits.shiftR todo shiftedCode .&. (2^n-1) of
                 0 -> do
                    guard $ todo == 0
                    return []
                 set -> do
                    d <- [0 .. n-1]
                    guard $ Bits.testBit set d
                    rest <-
                       let newCode = shiftedCode + d
                       in  go newCode $ Bits.clearBit todo newCode
                    return $ d:rest
   in  map (take (n^k) . (replicate k 0 ++)) $
       go 0 $ (2^n^k-2 :: Integer)


-- * tests

testLexLeast :: Int -> Int -> Bool
testLexLeast n k =
   lexLeast n k == head (allMap n k)

test :: Int -> Int -> [Int] -> Bool
test n k xs =
   replicateM k [0 .. n-1]
   ==
   (List.sort $ Match.take xs $ map (take k) $ List.tails $ cycle xs)

testAll :: Int -> Int -> Bool
testAll n k =
   List.all (test n k) $ allMap n k

-- Do not edit! Automatically created with doctest-extract from src/Combinatorics.hs
{-# LINE 53 "src/Combinatorics.hs" #-}

module Test.Combinatorics where

import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 54 "src/Combinatorics.hs" #-}
import     qualified Combinatorics as Comb
import     qualified Test.QuickCheck as QC
import     Test.Utility (equalFuncList, equalFuncList2)

import     Control.Applicative (liftA2, (<$>))
import     qualified Data.List.Match as Match
import     qualified Data.List.Key as Key
import     qualified Data.List as List
import     qualified Data.Set as Set
import     Data.Tuple.HT (uncurry3)
import     Data.List.HT (allEqual, isAscending)
import     Data.List (sort, nub)
import     Data.Eq.HT (equating)

genPermuteRep     :: Int -> QC.Gen [(Char, Int)]
genPermuteRep     n = do
       xns <- QC.listOf $ liftA2 (,) QC.arbitrary $ QC.choose (0,n)
       return $ Match.take (takeWhile (<=n) $ scanl1 (+) $ map snd xns) xns

genVariate     :: QC.Gen [Char]
genVariate     = take 7 <$> QC.arbitrary

genBinomial     :: QC.Gen (Integer, Integer)
genBinomial     = do
       n <- QC.choose (0,100)
       k <- QC.choose (0,n)
       return (n,k)

genChooseIndex     :: QC.Gen (Integer, Integer, Integer)
genChooseIndex     = do
       n <- QC.choose (0,25)
       k <- QC.choose (0,n)
       i <- QC.choose (0, Comb.binomial n k - 1)
       return (n,k,i)

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Combinatorics:105: "
{-# LINE 105 "src/Combinatorics.hs" #-}
 DocTest.property
{-# LINE 105 "src/Combinatorics.hs" #-}
     (QC.forAll (take 6 <$> QC.arbitrary :: QC.Gen [Int]) $ \xs -> allEqual $ map (\p -> sort (p xs)) $ Comb.permute : Comb.permuteFast : Comb.permuteShare : [])
 DocTest.printPrefix "Combinatorics:100: "
{-# LINE 100 "src/Combinatorics.hs" #-}
 DocTest.example
{-# LINE 100 "src/Combinatorics.hs" #-}
   (Comb.permute "abc")
  [ExpectedLine [LineChunk "[\"abc\",\"acb\",\"bac\",\"bca\",\"cab\",\"cba\"]"]]
 DocTest.printPrefix "Combinatorics:102: "
{-# LINE 102 "src/Combinatorics.hs" #-}
 DocTest.example
{-# LINE 102 "src/Combinatorics.hs" #-}
   (Comb.permute "aabc")
  [ExpectedLine [LineChunk "[\"aabc\",\"aacb\",\"abac\",\"abca\",\"acab\",\"acba\",\"aabc\",\"aacb\",\"abac\",\"abca\",\"acab\",\"acba\",\"baac\",\"baca\",\"baac\",\"baca\",\"bcaa\",\"bcaa\",\"caab\",\"caba\",\"caab\",\"caba\",\"cbaa\",\"cbaa\"]"]]
 DocTest.printPrefix "Combinatorics:149: "
{-# LINE 149 "src/Combinatorics.hs" #-}
 DocTest.property
{-# LINE 149 "src/Combinatorics.hs" #-}
     (QC.forAll (genPermuteRep  7) $ \xs -> let perms = Comb.permuteRep $ Key.nub fst xs in perms == nub perms)
 DocTest.printPrefix "Combinatorics:150: "
{-# LINE 150 "src/Combinatorics.hs" #-}
 DocTest.property
{-# LINE 150 "src/Combinatorics.hs" #-}
     (QC.forAll (genPermuteRep 10) $ \xs -> let perms = Comb.permuteRep $ Key.nub fst xs in List.sort perms == Set.toList (Set.fromList perms))
 DocTest.printPrefix "Combinatorics:151: "
{-# LINE 151 "src/Combinatorics.hs" #-}
 DocTest.property
{-# LINE 151 "src/Combinatorics.hs" #-}
     (QC.forAll (genPermuteRep 10) $ isAscending . Comb.permuteRep . Key.nub fst . sort)
 DocTest.printPrefix "Combinatorics:152: "
{-# LINE 152 "src/Combinatorics.hs" #-}
 DocTest.property
{-# LINE 152 "src/Combinatorics.hs" #-}
     (QC.forAll (QC.choose (0,10)) $ \n k -> Comb.choose n k == Comb.permuteRep [(False, n-k), (True, k)])
 DocTest.printPrefix "Combinatorics:146: "
{-# LINE 146 "src/Combinatorics.hs" #-}
 DocTest.example
{-# LINE 146 "src/Combinatorics.hs" #-}
   (Comb.permuteRep [('a',2), ('b',1), ('c',1)])
  [ExpectedLine [LineChunk "[\"aabc\",\"aacb\",\"abac\",\"abca\",\"acab\",\"acba\",\"baac\",\"baca\",\"bcaa\",\"caab\",\"caba\",\"cbaa\"]"]]
 DocTest.printPrefix "Combinatorics:164: "
{-# LINE 164 "src/Combinatorics.hs" #-}
 DocTest.property
{-# LINE 164 "src/Combinatorics.hs" #-}
     (QC.forAll (QC.choose (0,10)) $ \n k -> all (\x  ->  n == length x  &&  k == length (filter id x)) (Comb.choose n k))
 DocTest.printPrefix "Combinatorics:159: "
{-# LINE 159 "src/Combinatorics.hs" #-}
 DocTest.example
{-# LINE 159 "src/Combinatorics.hs" #-}
   (map (map (\b -> if b then 'x' else '.')) $ Comb.choose 5 3)
  [ExpectedLine [LineChunk "[\"..xxx\",\".x.xx\",\".xx.x\",\".xxx.\",\"x..xx\",\"x.x.x\",\"x.xx.\",\"xx..x\",\"xx.x.\",\"xxx..\"]"]]
 DocTest.printPrefix "Combinatorics:161: "
{-# LINE 161 "src/Combinatorics.hs" #-}
 DocTest.example
{-# LINE 161 "src/Combinatorics.hs" #-}
   (map (map (\b -> if b then 'x' else '.')) $ Comb.choose 3 5)
  [ExpectedLine [LineChunk "[]"]]
 DocTest.printPrefix "Combinatorics:175: "
{-# LINE 175 "src/Combinatorics.hs" #-}
 DocTest.example
{-# LINE 175 "src/Combinatorics.hs" #-}
   (Comb.variateRep 2 "abc")
  [ExpectedLine [LineChunk "[\"aa\",\"ab\",\"ac\",\"ba\",\"bb\",\"bc\",\"ca\",\"cb\",\"cc\"]"]]
 DocTest.printPrefix "Combinatorics:192: "
{-# LINE 192 "src/Combinatorics.hs" #-}
 DocTest.property
{-# LINE 192 "src/Combinatorics.hs" #-}
     (QC.forAll genVariate $ \xs -> Comb.variate (length xs) xs == Comb.permute xs)
 DocTest.printPrefix "Combinatorics:193: "
{-# LINE 193 "src/Combinatorics.hs" #-}
 DocTest.property
{-# LINE 193 "src/Combinatorics.hs" #-}
     (\xs -> equating (take 1000) (Comb.variate (length xs) xs) (Comb.permute (xs::String)))
 DocTest.printPrefix "Combinatorics:185: "
{-# LINE 185 "src/Combinatorics.hs" #-}
 DocTest.example
{-# LINE 185 "src/Combinatorics.hs" #-}
   (Comb.variate 2 "abc")
  [ExpectedLine [LineChunk "[\"ab\",\"ac\",\"ba\",\"bc\",\"ca\",\"cb\"]"]]
 DocTest.printPrefix "Combinatorics:187: "
{-# LINE 187 "src/Combinatorics.hs" #-}
 DocTest.example
{-# LINE 187 "src/Combinatorics.hs" #-}
   (Comb.variate 2 "abcd")
  [ExpectedLine [LineChunk "[\"ab\",\"ac\",\"ad\",\"ba\",\"bc\",\"bd\",\"ca\",\"cb\",\"cd\",\"da\",\"db\",\"dc\"]"]]
 DocTest.printPrefix "Combinatorics:189: "
{-# LINE 189 "src/Combinatorics.hs" #-}
 DocTest.example
{-# LINE 189 "src/Combinatorics.hs" #-}
   (Comb.variate 3 "abcd")
  [ExpectedLine [LineChunk "[\"abc\",\"abd\",\"acb\",\"acd\",\"adb\",\"adc\",\"bac\",\"bad\",\"bca\",\"bcd\",\"bda\",\"bdc\",\"cab\",\"cad\",\"cba\",\"cbd\",\"cda\",\"cdb\",\"dab\",\"dac\",\"dba\",\"dbc\",\"dca\",\"dcb\"]"]]
 DocTest.printPrefix "Combinatorics:203: "
{-# LINE 203 "src/Combinatorics.hs" #-}
 DocTest.example
{-# LINE 203 "src/Combinatorics.hs" #-}
   (Comb.tuples 2 "abc")
  [ExpectedLine [LineChunk "[\"ab\",\"ac\",\"bc\"]"]]
 DocTest.printPrefix "Combinatorics:205: "
{-# LINE 205 "src/Combinatorics.hs" #-}
 DocTest.example
{-# LINE 205 "src/Combinatorics.hs" #-}
   (Comb.tuples 2 "abcd")
  [ExpectedLine [LineChunk "[\"ab\",\"ac\",\"ad\",\"bc\",\"bd\",\"cd\"]"]]
 DocTest.printPrefix "Combinatorics:207: "
{-# LINE 207 "src/Combinatorics.hs" #-}
 DocTest.example
{-# LINE 207 "src/Combinatorics.hs" #-}
   (Comb.tuples 3 "abcd")
  [ExpectedLine [LineChunk "[\"abc\",\"abd\",\"acd\",\"bcd\"]"]]
 DocTest.printPrefix "Combinatorics:218: "
{-# LINE 218 "src/Combinatorics.hs" #-}
 DocTest.property
{-# LINE 218 "src/Combinatorics.hs" #-}
     (QC.forAll genVariate $ \xs -> length (Comb.partitions xs)  ==  2 ^ length xs)
 DocTest.printPrefix "Combinatorics:215: "
{-# LINE 215 "src/Combinatorics.hs" #-}
 DocTest.example
{-# LINE 215 "src/Combinatorics.hs" #-}
   (Comb.partitions "abc")
  [ExpectedLine [LineChunk "[(\"abc\",\"\"),(\"bc\",\"a\"),(\"ac\",\"b\"),(\"c\",\"ab\"),(\"ab\",\"c\"),(\"b\",\"ac\"),(\"a\",\"bc\"),(\"\",\"abc\")]"]]
 DocTest.printPrefix "Combinatorics:240: "
{-# LINE 240 "src/Combinatorics.hs" #-}
 DocTest.property
{-# LINE 240 "src/Combinatorics.hs" #-}
     (QC.forAll (QC.choose (0,7)) $ \k xs -> isAscending . Comb.rectifications k . nub . sort $ (xs::String))
 DocTest.printPrefix "Combinatorics:235: "
{-# LINE 235 "src/Combinatorics.hs" #-}
 DocTest.example
{-# LINE 235 "src/Combinatorics.hs" #-}
   (Comb.rectifications 4 "abc")
  [ExpectedLine [LineChunk "[\"aabc\",\"abac\",\"abbc\",\"abca\",\"abcb\",\"abcc\"]"]]
 DocTest.printPrefix "Combinatorics:237: "
{-# LINE 237 "src/Combinatorics.hs" #-}
 DocTest.example
{-# LINE 237 "src/Combinatorics.hs" #-}
   (map (length . uncurry Comb.rectifications) $ do x<-[0..10]; y<-[0..x]; return (x,[1..y::Int]))
  [ExpectedLine [LineChunk "[1,0,1,0,1,1,0,1,3,1,0,1,7,6,1,0,1,15,25,10,1,0,1,31,90,65,15,1,0,1,63,301,350,140,21,1,0,1,127,966,1701,1050,266,28,1,0,1,255,3025,7770,6951,2646,462,36,1,0,1,511,9330,34105,42525,22827,5880,750,45,1]"]]
 DocTest.printPrefix "Combinatorics:316: "
{-# LINE 316 "src/Combinatorics.hs" #-}
 DocTest.property
{-# LINE 316 "src/Combinatorics.hs" #-}
     (QC.forAll (QC.choose (0,10)) $ \n k -> map (Comb.chooseUnrank n k) [0 .. Comb.binomial n k - 1]  ==  Comb.choose n k)
 DocTest.printPrefix "Combinatorics:317: "
{-# LINE 317 "src/Combinatorics.hs" #-}
 DocTest.property
{-# LINE 317 "src/Combinatorics.hs" #-}
     (QC.forAll genChooseIndex $ \(n,k,i) -> Comb.chooseRank (Comb.chooseUnrank n k i)  ==  (n, k, i))
 DocTest.printPrefix "Combinatorics:318: "
{-# LINE 318 "src/Combinatorics.hs" #-}
 DocTest.property
{-# LINE 318 "src/Combinatorics.hs" #-}
     (\bs -> uncurry3 Comb.chooseUnrank (Comb.chooseRank bs :: (Integer, Integer, Integer))  ==  bs)
 DocTest.printPrefix "Combinatorics:349: "
{-# LINE 349 "src/Combinatorics.hs" #-}
 DocTest.property
{-# LINE 349 "src/Combinatorics.hs" #-}
     (QC.forAll (take 8 <$> QC.arbitrary) $ \xs -> length (Comb.permute xs) == Comb.factorial (length (xs::String)))
 DocTest.printPrefix "Combinatorics:350: "
{-# LINE 350 "src/Combinatorics.hs" #-}
 DocTest.property
{-# LINE 350 "src/Combinatorics.hs" #-}
     (QC.forAll (take 6 <$> QC.arbitrary) $ \xs -> sum (map sum (Comb.permute xs)) == sum xs * Comb.factorial (length xs))
 DocTest.printPrefix "Combinatorics:358: "
{-# LINE 358 "src/Combinatorics.hs" #-}
 DocTest.property
{-# LINE 358 "src/Combinatorics.hs" #-}
     (QC.forAll (QC.choose (0,12)) $ \n k -> length (Comb.choose n k) == Comb.binomial n k)
 DocTest.printPrefix "Combinatorics:359: "
{-# LINE 359 "src/Combinatorics.hs" #-}
 DocTest.property
{-# LINE 359 "src/Combinatorics.hs" #-}
     (QC.forAll genBinomial $ \(n,k) -> let (q, r) = divMod (Comb.factorial n) (Comb.factorial k * Comb.factorial (n-k)) in r == 0 && Comb.binomial n k == q)
 DocTest.printPrefix "Combinatorics:360: "
{-# LINE 360 "src/Combinatorics.hs" #-}
 DocTest.property
{-# LINE 360 "src/Combinatorics.hs" #-}
     (QC.forAll (take 16 <$> QC.arbitrary) $ \xs k -> length (Comb.tuples k xs) == Comb.binomial (length (xs::String)) k)
 DocTest.printPrefix "Combinatorics:379: "
{-# LINE 379 "src/Combinatorics.hs" #-}
 DocTest.property
{-# LINE 379 "src/Combinatorics.hs" #-}
     (QC.forAll (genPermuteRep 10) $ \xs -> length (Comb.permuteRep xs) == Comb.multinomial (map snd xs))
 DocTest.printPrefix "Combinatorics:380: "
{-# LINE 380 "src/Combinatorics.hs" #-}
 DocTest.property
{-# LINE 380 "src/Combinatorics.hs" #-}
     (QC.forAll (QC.listOf $ QC.choose (0,300::Integer)) $ \xs -> Comb.multinomial xs == Comb.multinomial (sort xs))
 DocTest.printPrefix "Combinatorics:390: "
{-# LINE 390 "src/Combinatorics.hs" #-}
 DocTest.property
{-# LINE 390 "src/Combinatorics.hs" #-}
     (equalFuncList Comb.factorial Comb.factorials 1000)
 DocTest.printPrefix "Combinatorics:401: "
{-# LINE 401 "src/Combinatorics.hs" #-}
 DocTest.property
{-# LINE 401 "src/Combinatorics.hs" #-}
     (equalFuncList2 Comb.binomial Comb.binomials 100)
 DocTest.printPrefix "Combinatorics:420: "
{-# LINE 420 "src/Combinatorics.hs" #-}
 DocTest.property
{-# LINE 420 "src/Combinatorics.hs" #-}
     (equalFuncList Comb.catalanNumber Comb.catalanNumbers 1000)
 DocTest.printPrefix "Combinatorics:438: "
{-# LINE 438 "src/Combinatorics.hs" #-}
 DocTest.property
{-# LINE 438 "src/Combinatorics.hs" #-}
     (equalFuncList Comb.derangementNumber Comb.derangementNumbers 1000)
 DocTest.printPrefix "Combinatorics:450: "
{-# LINE 450 "src/Combinatorics.hs" #-}
 DocTest.property
{-# LINE 450 "src/Combinatorics.hs" #-}
     (QC.forAll (QC.choose (0,10000)) $ \k -> QC.forAll (take 7 <$> QC.arbitrary) $ \xs -> length (Comb.setPartitions k xs) == (Comb.setPartitionNumbers !! length (xs::String) ++ repeat 0) !! k)
 DocTest.printPrefix "Combinatorics:451: "
{-# LINE 451 "src/Combinatorics.hs" #-}
 DocTest.property
{-# LINE 451 "src/Combinatorics.hs" #-}
     (QC.forAll (QC.choose (0,7)) $ \k xs -> length (Comb.rectifications k xs) == (Comb.setPartitionNumbers !! k ++ repeat 0) !! length (xs::String))
 DocTest.printPrefix "Combinatorics:471: "
{-# LINE 471 "src/Combinatorics.hs" #-}
 DocTest.property
{-# LINE 471 "src/Combinatorics.hs" #-}
     (equalFuncList2 Comb.surjectiveMappingNumber Comb.surjectiveMappingNumbers 20)
 DocTest.printPrefix "Combinatorics:525: "
{-# LINE 525 "src/Combinatorics.hs" #-}
 DocTest.property
{-# LINE 525 "src/Combinatorics.hs" #-}
     (equalFuncList Comb.fibonacciNumber Comb.fibonacciNumbers 10000)

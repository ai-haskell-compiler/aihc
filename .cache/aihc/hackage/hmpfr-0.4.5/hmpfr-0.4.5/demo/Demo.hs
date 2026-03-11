module Main where

import qualified Data.Number.MPFR as M --import functions
import Data.Number.MPFR.Instances.Up -- import instances

import qualified Data.Number.MPFR.Mutable as MM

import Control.Monad.ST(runST, ST)

-- compute the sum from 1 to n with precision of p bits rounded to Near
s1     :: M.Precision -> Int -> M.MPFR
s1 p n = s1' 1 0
    where s1' k acc | k <= n = s1' (succ k) (M.add M.Near p acc (M.fromInt M.Near 32 k))
                    | otherwise = acc

-- or the same using addi + foldl instead of add
s2    :: M.Precision -> Int -> M.MPFR
s2 p  = foldl (M.addi M.Near p) 0 . enumFromTo 1

-- or the same as s1 except with foldl
s3 :: M.Precision -> Int -> M.MPFR
s3 p = foldl ((. M.fromInt M.Up p) . (+)) M.zero . enumFromTo 1

-- or idiomatically using the MPFR Num instance
-- note that this version is a lot slower than previous three
-- guess why :)
s4 :: M.Precision -> Int -> M.MPFR
s4 p = sum . map (M.fromInt M.Up p) . enumFromTo 1

-- or with mutable MPFR
s5 p n = runST $ go n =<< MM.unsafeThaw (M.fromInt M.Near p 0)
    where go 0 acc = MM.unsafeFreeze acc
          go m acc = MM.addi acc acc m M.Near >> go (m-1) acc

-- or, if you're feeling haskelly
s6 p n = runST $ MM.unsafeThaw (M.fromInt M.Near p 0) >>=
         \acc -> mapM_ (flip (MM.addi acc acc) M.Near) [1..n] >>
         MM.unsafeFreeze acc

-- sum up first n terms of a Taylor series for e with precision p
e1     :: M.Precision -> Int -> M.MPFR
e1 p n = e' 1 1 1
    where e' k acc acc' | k == n+1 = acc
                        | otherwise= e' (succ k) (M.add M.Up p acc acc'') acc''
                        where acc'' = M.divi M.Up p acc' k

-- or using mutable MPFR's
e2     :: M.Precision -> Int -> M.MPFR
e2 p n = let one = M.fromInt M.Near p 1
         in runST $ do acc <- MM.unsafeThaw one
                       acc' <- MM.thaw one
                       mapM_ ((>> MM.add acc acc acc' M.Up)
                              . flip (MM.divi acc' acc') M.Up) [1..n]
                       MM.unsafeFreeze acc

testRandom =
    do
    let rsP = M.newRandomStatePointer
    print $ M.urandomb rsP 1000
    print $ M.urandomb rsP 1000
    print $ M.urandomb rsP 1000
    print $ M.urandomb rsP 1000

main = do print $ s1 1000 100000
          print $ s6 1000 100000
          print $ e1 1000 100000
          print $ e2 1000 100000
          testRandom
          let c1 = one
          putStrLn $ "exp 1 = " ++ (M.toStringExp 10000 $ M.exp M.Up 100 c1)
          putStrLn $ "exp -1 = " ++ (M.toStringExp 10000 $ M.exp M.Up 100 (M.neg M.Up 100 c1))
    
one = 1 :: M.MPFR
          

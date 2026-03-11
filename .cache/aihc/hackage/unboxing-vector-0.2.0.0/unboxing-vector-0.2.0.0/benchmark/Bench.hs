{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-type-defaults -Wno-name-shadowing #-}
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Unboxing as V
import qualified Data.Vector as B
import qualified Data.Vector.Generic as G
import System.Environment (getArgs, getProgName)
import Control.Monad
import Data.List
import qualified Poly as P

modulo :: Int
modulo = 17

newtype IntMod = IntMod Int deriving (Eq)

instance Show IntMod where
  show (IntMod n) = show n

instance V.Unboxable IntMod where
  type Rep IntMod = Int

instance Num IntMod where
  IntMod x + IntMod y = IntMod ((x + y) `rem` modulo)
  IntMod x - IntMod y = IntMod ((x - y) `mod` modulo)
  IntMod x * IntMod y = IntMod ((x * y) `rem` modulo)
  negate (IntMod x) = IntMod (negate x `mod` modulo)
  fromInteger n = IntMod (fromInteger (n `mod` fromIntegral modulo))
  abs = undefined; signum = undefined

instance Fractional IntMod where
  recip x = let x2 = x * x
                x5 = x2 * x2 * x
                x15 = x5 * x5 * x5
            in x15 -- x^15
  fromRational = undefined

trim :: U.Vector Int -> U.Vector Int
trim xs
  | U.null xs = U.empty
  | U.last xs == 0 = trim (U.init xs)
  | otherwise = xs

addMod, subMod, mulMod :: Int -> Int -> Int
addMod x y = (x + y) `rem` modulo
subMod x y = (x - y) `mod` modulo
mulMod x y = (x * y) `rem` modulo

sumMod :: [Int] -> Int
sumMod = foldl' addMod 0

negateMod :: Int -> Int
negateMod x = (negate x) `mod` modulo

recipMod :: Int -> Int
recipMod x = let x2 = x `mulMod` x
                 x5 = (x2 `mulMod` x2) `mulMod` x
                 x15 = (x5 `mulMod` x5) `mulMod` x5
             in x15

polyAdd :: U.Vector Int -> U.Vector Int -> U.Vector Int
polyAdd xs ys
  | n < m = U.create $ do
      v <- UM.new m
      forM_ [0..n-1] $ \i -> UM.write v i ((xs U.! i) `addMod` (ys U.! i))
      forM_ [n..m-1] $ \i -> UM.write v i (ys U.! i)
      return v
  | m < n = U.create $ do
      v <- UM.new n
      forM_ [0..m-1] $ \i -> UM.write v i ((xs U.! i) `addMod` (ys U.! i))
      forM_ [m..n-1] $ \i -> UM.write v i (xs U.! i)
      return v
  | otherwise = trim $ U.zipWith addMod xs ys
  where n = U.length xs
        m = U.length ys

polySub :: U.Vector Int -> U.Vector Int -> U.Vector Int
polySub xs ys
  | n < m = U.create $ do
      v <- UM.new m
      forM_ [0..n-1] $ \i -> UM.write v i ((xs U.! i) `subMod` (ys U.! i))
      forM_ [n..m-1] $ \i -> UM.write v i (negateMod (ys U.! i))
      return v
  | m < n = U.create $ do
      v <- UM.new n
      forM_ [0..m-1] $ \i -> UM.write v i ((xs U.! i) `subMod` (ys U.! i))
      forM_ [m..n-1] $ \i -> UM.write v i (xs U.! i)
      return v
  | otherwise = trim $ U.zipWith subMod xs ys
  where n = U.length xs
        m = U.length ys

polyMul :: U.Vector Int -> U.Vector Int -> U.Vector Int
polyMul xs ys
    | n == 0 || m == 0 = U.empty
    | otherwise = U.generate (n + m - 1) (\i -> sumMod [(xs U.! j) `mulMod` (ys U.! (i - j)) | j <- [max 0 (i - m + 1)..min i (n-1)]])
    where n = U.length xs
          m = U.length ys

scalePoly :: Int -> U.Vector Int -> U.Vector Int
scalePoly a xs
  | a == 0 = U.empty
  | otherwise = U.map (`mulMod` a) xs

toMonicPoly :: U.Vector Int -> U.Vector Int
toMonicPoly xs | U.null xs = U.empty
               | otherwise = U.map (`mulMod` recipMod (U.last xs)) xs

divModPoly :: U.Vector Int -> U.Vector Int -> (U.Vector Int, U.Vector Int)
divModPoly f g
  | U.null g = error "divMod: divide by zero"
  | U.length f < U.length g = (U.empty, f)
  | otherwise = loop U.empty (scalePoly (recipMod b) f)
  where
    g' = toMonicPoly g
    b = U.last g
    -- invariant: f == q * g + scale b p
    loop q !p | U.length p < U.length g = (q, scalePoly b p)
              | otherwise = let !q' = U.drop (U.length g - 1) p
                            in loop (q `polyAdd` q') (p `polySub` (q' `polyMul` g'))

modPoly :: U.Vector Int -> U.Vector Int -> U.Vector Int
modPoly f g = snd (divModPoly f g)

powModPoly :: U.Vector Int -> Int -> U.Vector Int -> U.Vector Int
powModPoly _ 0 _modulo = U.singleton 1
powModPoly f n modulo = loop (n-1) f f
  where loop 0 !_ !acc = acc
        loop 1 !m !acc = (m `polyMul` acc) `modPoly` modulo
        loop i !m !acc
          | even i = loop (i `quot` 2) ((m `polyMul` m) `modPoly` modulo) acc
          | otherwise = loop (i `quot` 2) ((m `polyMul` m) `modPoly` modulo) ((m `polyMul` acc) `modPoly` modulo)

powPoly :: U.Vector Int -> Int -> U.Vector Int
powPoly _ 0 = U.singleton 1
powPoly f n = loop (n-1) f f
  where loop 0 !_ !acc = acc
        loop 1 !m !acc = m `polyMul` acc
        loop i !m !acc
          | even i = loop (i `quot` 2) (m `polyMul` m) acc
          | otherwise = loop (i `quot` 2) (m `polyMul` m) (m `polyMul` acc)

-- Specialization for unboxing vectors + IntMod:
{-# SPECIALIZE P.addPoly :: P.Poly V.Vector IntMod -> P.Poly V.Vector IntMod -> P.Poly V.Vector IntMod #-}
{-# SPECIALIZE P.subPoly :: P.Poly V.Vector IntMod -> P.Poly V.Vector IntMod -> P.Poly V.Vector IntMod #-}
{-# SPECIALIZE P.mulPoly :: P.Poly V.Vector IntMod -> P.Poly V.Vector IntMod -> P.Poly V.Vector IntMod #-}
{-# SPECIALIZE P.divMod :: P.Poly V.Vector IntMod -> P.Poly V.Vector IntMod -> (P.Poly V.Vector IntMod, P.Poly V.Vector IntMod) #-}
{-# SPECIALIZE P.powMod :: P.Poly V.Vector IntMod -> Int -> P.Poly V.Vector IntMod -> P.Poly V.Vector IntMod #-}

-- Specialization for boxed vectors + IntMod:
{-# SPECIALIZE P.addPoly :: P.Poly B.Vector IntMod -> P.Poly B.Vector IntMod -> P.Poly B.Vector IntMod #-}
{-# SPECIALIZE P.subPoly :: P.Poly B.Vector IntMod -> P.Poly B.Vector IntMod -> P.Poly B.Vector IntMod #-}
{-# SPECIALIZE P.mulPoly :: P.Poly B.Vector IntMod -> P.Poly B.Vector IntMod -> P.Poly B.Vector IntMod #-}
{-# SPECIALIZE P.divMod :: P.Poly B.Vector IntMod -> P.Poly B.Vector IntMod -> (P.Poly B.Vector IntMod, P.Poly B.Vector IntMod) #-}
{-# SPECIALIZE P.powMod :: P.Poly B.Vector IntMod -> Int -> P.Poly B.Vector IntMod -> P.Poly B.Vector IntMod #-}

main :: IO ()
main = do
  args <- getArgs
  case args of
    "unboxed":_ -> do
      let f = U.fromList [modulo-1,modulo-1,modulo-1] `polyAdd` (powPoly (U.fromList [0,1]) 2000) {- U.fromList [if k==2000 then 1 else 0 | k<-[0..2000]] -}
      let g = powModPoly (U.fromList [0,1]) 1000000000 f
      print $ sumMod $ U.toList g -- should print '1'
    "unboxing":_ -> do
      let f = P.x^2000 - (P.x^2 + P.x + 1) :: P.Poly V.Vector IntMod
      let g = P.powMod P.x 1000000000 f
      print $ G.sum $ P.coeffAsc g -- should print '1'
    "boxed":_ -> do
      let f = P.x^2000 - (P.x^2 + P.x + 1) :: P.Poly B.Vector IntMod
      let g = P.powMod P.x 1000000000 f
      print $ G.sum $ P.coeffAsc g -- should print '1'
    _ -> do
      progName <- getProgName
      putStrLn $ progName ++ " (unboxed|unboxing|boxed)"
      putStr $ unlines ["This program computes the polynomial x^1000000000 mod (x^2000 - x^2 - x - 1)"
                       ,"and prints its value at x=1 in the finite field F_17 (or GF(17))."
                       ,"Run with '+RTS -t' to show memory usage."
                       ]

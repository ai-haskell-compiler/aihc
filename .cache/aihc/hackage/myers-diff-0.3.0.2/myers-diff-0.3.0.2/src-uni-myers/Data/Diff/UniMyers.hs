
-- Taken this code from https://hackage.haskell.org/package/uni-util, in the Util.Myers module
-- This code is under the LGPL.

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Implementation of the Myers algorithm, from "An O(ND) Difference Algorithm
-- and Its Variations", by Eugene Myers page 6 (figure 2).
--
-- Specification: if
--
--    f1 (InBoth v) = Just v
--    f1 (InFirst v) = Just v
--    f1 (InSecond v) = Nothing
--
-- and
--
--    f2 (InBoth v) = Just v
--    f2 (InFirst v) = Nothing
--    f2 (InSecond v) = Just v
--
-- then
--
--    mapPartial f1 (diff l1 l2) == l1
--
-- and
--
--    mapPartial f2 (diff l1 l2) == l2
module Data.Diff.UniMyers (
   diff,
   diff2,
   DiffElement(..),

   utilDiff,
   utilDiffToLspDiff
   ) where


import Control.Monad.ST
import Data.Array hiding (index)
import Data.Array.ST hiding (index)
import Data.Diff.Types
import Data.Function
import qualified Data.List as L
import qualified Data.Text as T

-- import Util.ExtendedPrelude

-- -----------------------------------------------------------------------
-- Datatypes
-- -----------------------------------------------------------------------

data DiffElement v =
      InBoth [v]
   |  InFirst [v]
   |  InSecond [v] deriving (Show)

-- -----------------------------------------------------------------------
-- The implementation.  The whole function, apart from the body of diff
-- itself, is taken from a message from Andrew Bromage
-- -----------------------------------------------------------------------

diff :: (Eq a) => [a] -> [a] -> [DiffElement a]
diff l1 l2 =
   let
      common = lcss l1 l2

      addFirst :: [a] -> [DiffElement a] -> [DiffElement a]
      addFirst [] de0 = de0
      addFirst l1 de0 = InFirst l1 : de0

      addSecond :: [a] -> [DiffElement a] -> [DiffElement a]
      addSecond [] de0 = de0
      addSecond l1 de0 = InSecond l1 : de0

      doCommon :: Eq a => [a] -> [a] -> [a] -> [DiffElement a]
      doCommon [] l1 l2 = (addFirst l1) . (addSecond l2) $ []
      doCommon (c:cs) l10 l20 =
         let
            Just (l1A,l11) = splitToElem (== c) l10
            Just (l2A,l21) = splitToElem (== c) l20
            de0 = doCommon cs l11 l21
            de1 = case de0 of
               (InBoth cs:rest) -> InBoth (c:cs):rest
               _ -> InBoth [c] : de0
         in
             (addFirst l1A) . (addSecond l2A) $ de1
   in
      doCommon common l1 l2

-- stolen from message from Andrew Bromage
algb :: (Eq a) => [a] -> [a] -> [Int]
algb xs ys
  = 0 : algb1 xs [ (y,0) | y <- ys ]
  where
    algb1 [] ys' = map snd ys'
    algb1 (x:xs) ys'
      = algb1 xs (algb2 0 0 ys')
      where
        algb2 _ _ [] = []
        algb2 k0j1 k1j1 ((y,k0j):ys)
          = let kjcurr = if x == y then k0j1+1 else max k1j1 k0j
            in (y,kjcurr) : algb2 k0j kjcurr ys

algc :: (Eq a) => Int -> Int -> [a] -> [a] -> [a] -> [a]
algc m n xs []  = id
algc m n [x] ys = if x `elem` ys then (x:) else id
algc m n xs ys
  = algc m2 k xs1 (take k ys) . algc (m-m2) (n-k) xs2 (drop k ys)
  where
    m2 = m `div` 2

    xs1 = take m2 xs
    xs2 = drop m2 xs

    l1 = algb xs1 ys
    l2 = reverse (algb (reverse xs2) (reverse ys))

    k = findk 0 0 (-1) (zip l1 l2)

    findk k km m [] = km
    findk k km m ((x,y):xys)
      | x+y >= m  = findk (k+1) k  (x+y) xys
      | otherwise = findk (k+1) km m     xys

lcss :: (Eq a) => [a] -> [a] -> [a]
lcss xs ys = algc (length xs) (length ys) xs ys []


{- Here, as an appendix, is my slow inefficient version -}
diff2 :: Eq v => [v] -> [v] -> [DiffElement v]
diff2 [] [] = []
diff2 a b = runST (diffST2 a b)

-- NB. diffST does not work if both arguments are null, so that
-- case should be handled separately.
diffST2 :: forall v s . Eq v => [v] -> [v] -> ST s [DiffElement v]
diffST2 a b =
   do
      let
         m = length a
         (aArr :: Array Int v) = listArray (1,m) a

         n = length b
         (bArr :: Array Int v) = listArray (1,n) b

         match :: Int -> Int -> Bool
         match x y = (aArr ! x) == (bArr ! y)

         -- Given (x,y) return the highest (x+k,y+k) such that (x+1,y+1),
         -- (x+2,y+2)...(x+k,y+k) match.
         scan :: Int -> Int -> (Int,Int)
         scan x y =
            if x < m && y < n
               then
                  let
                     x' = x+1
                     y' = y+1
                  in
                     if match x' y' then scan x' y' else (x,y)
               else
                  (x,y)

         max = m+n
      -- We do the computation using an STArray for V
      -- We arrange that there is always a -1 on either side of the
      -- existing range, to simplify handling of the end-cases.
      (v :: STUArray s Int Int) <- newArray (-max-1,max+1) (-1)
      writeArray v 1 0

      -- The w array contains a list of integers (x,y) such that the snakes
      -- starting from the elements (x+1,y+1) together make up all the snakes
      -- needed in the optimal solution.
      --
      -- The idea is that storage for w should not get too big, either if a
      -- and b are much the same, or if they are completely different.  Thus
      -- in most cases quadratic behaviour *should* be avoided.
      (w :: STArray s Int [(Int,Int)]) <- newArray (-max,max) []

      let
         -- step carries out the algorithm for a given (d,k), returning
         -- the appropriate w-list.
         step :: Int -> Int -> ST s [(Int,Int)]
         step d k =
            if k > d
               then
                  innerStep (d+1) (-(d+1))
               else
                  innerStep d k

         innerStep :: Int -> Int -> ST s [(Int,Int)]
         innerStep d k =
            do
               vkplus <- readArray v (k+1)
               vkminus <- readArray v (k-1)
               (x,l0) <- if vkminus < vkplus
                  then
                     do
                        l0 <- readArray w (k+1)
                        return (vkplus,l0)
                  else
                     do
                        l <- readArray w (k-1)
                        return (vkminus+1,l)
               let
                  y = x - k

                  (x',_) = scan x y

                  l1 =
                     if x' == x
                        then
                           l0
                        else
                           (x,y) : l0

               -- Can we finish now?
               if x' >= m && (y + (x' - x)) >= n
                  then
                     return l1
                  else
                     do
                        writeArray v k x'
                        writeArray w k l1
                        step d (k+2)

      snakes <- step 0 0

      let
         -- The task is now to reassemble snakes to produce a list.  Since
         -- the snakes are given in reverse order, we may as well produce the
         -- elements in that order and work backwards.

         addSnake :: (Int,Int) -> (Int,Int)
            -> [DiffElement v] -> [DiffElement v]
         addSnake (lastX,lastY) (x,y) l0 =
            -- We assume that elements a[lastX+1...] and b[lastY+1...] have
            -- been dealt with, and we now add on a segment starting with a
            -- snake which begins at (x+1,y+1).
            let
               -- Compute the end of the snake
               (x',y') = scan x y

               -- Add on elements b[y'+1..lastY]
               l1 = (InSecond (map (\ index -> bArr ! index)
                       [y'+1..lastY])) : l0
               -- Add on elements a[x'+1..lastX]
               l2 = (InFirst (map (\ index -> aArr ! index)
                       [x'+1..lastX])) : l1
               -- Add on snake
               l3 = (InBoth (map (\ index -> aArr ! index)
                       [x+1..x'])) : l2
            in
               l3

         doSnakes :: (Int,Int) -> [(Int,Int)] -> [DiffElement v]
            -> [DiffElement v]
         doSnakes last [] l0 =
            -- we pretend there's a zero-length snake starting at (1,1).
            if last /= (0,0) then addSnake last (0,0) l0 else l0
         doSnakes last (s:ss) l0 =
            let
               l1 = addSnake last s l0
            in
               doSnakes s ss l1

         result0 = doSnakes (m,n) snakes []

         result1 = filter
            -- Filter out null elements
            (\ de -> case de of
               InFirst [] -> False
               InSecond [] -> False
               InBoth [] -> False
               _ -> True
               )
            result0

      return result1
{- -}


-- | This version was posted to the Haskell mailing list by Gertjan Kamsteeg
-- on Sun, 15 Dec 2002.
-- But it seems to be slightly slower than the others.
{-

data In a = F a | S a | B a deriving Show

diff xs ys = steps ([(0,0,[],xs,ys)],[]) where
  steps (((_,_,ws,[],[]):_),_) = reverse ws
  steps d                      = steps (step d) where
    step (ps,qs) = let (us,vs) = h1 ps in (h3 qs (h2 us),vs) where
      h1 []     = ([],[])
      h1 (p:ps) = let (rs,ss) = next p; (us,vs) = h1 ps in (rs++us,ss++vs)
         where
            next (k,n,ws,(x:xs),[])           = ([(k+1,n+1,F x:ws,xs,[])],[])
            next (k,n,ws,[],(y:ys))           = ([(k-1,n+1,S y:ws,[],ys)],[])
            next (k,n,ws,xs@(x:us),ys@(y:vs))
              | x == y    = ([],[(k,n+1,B x:ws,us,vs)])
              | otherwise = ([(k+1,n+1,F x:ws,us,ys),(k-1,n+1,S y:ws,xs,vs)],[])
      h2 []                                   = []
      h2 ps@[_]                               = ps
      h2 (p@(k1,n1,_,_,_):ps@(q@(k2,n2,_,_,_):us))
        | k1 == k2  = if n1 <= n2 then p:h2 us else q:h2 us
        | otherwise = p:h2 ps
      h3 ps [] = ps
      h3 [] qs = qs
      h3 (ps@(p@(k1,n1,_,_,_):us)) (qs@(q@(k2,n2,_,_,_):vs))
        | k1 > k2   = p:h3 us qs
        | k1 == k2  = if n1 <= n2 then p:h3 us vs else q:h3 us vs
        | otherwise = q:h3 ps vs
-}


splitToElem :: (a -> Bool) -> [a] -> Maybe ([a],[a])
splitToElem fn = sTC
   where
      sTC [] = Nothing
      sTC (x:xs) =
         if fn x then Just ([],xs) else
            fmap
               (\ (xs1,xs2) -> (x:xs1,xs2))
               (sTC xs)

-- * Shim


utilDiffToLspDiff :: [DiffElement Char] -> [ChangeEvent]
utilDiffToLspDiff elems = go [] 0 0 elems
  where
    go events curLine curChar ((InBoth chars):xs) = go events curLine' curChar' xs
      where
        curLine' = curLine + countNewlines chars
        curChar' = if hasNewline chars then lengthOfLastLine chars else curChar + (L.length chars)

    go events curLine curChar ((InFirst chars):xs) = go ((ChangeEvent (Range startPos endPos) ""):events) curLine' curChar' xs
      where
        startPos = Position curLine curChar
        endPos = Position (curLine + countNewlines chars) (if hasNewline chars then lengthOfLastLine chars else curChar + (L.length chars))

        curLine' = curLine
        curChar' = curChar

    go events curLine curChar ((InSecond chars):xs) = go ((ChangeEvent (Range startPos endPos) (T.pack chars)):events) curLine' curChar' xs
      where
        startPos = Position curLine curChar
        endPos = startPos

        curLine' = curLine + countNewlines chars
        curChar' = if hasNewline chars then lengthOfLastLine chars else curChar + (L.length chars)

    go events _curLine _curChar [] = reverse events

    hasNewline = any (== '\n')

    lengthOfLastLine chars = chars
                           & T.pack
                           & T.splitOn "\n"
                           & last
                           & T.length

    countNewlines = L.foldl' (\total c -> if c == '\n' then total + 1 else total) 0

utilDiff :: String -> String -> [ChangeEvent]
utilDiff s1 s2 = utilDiffToLspDiff (diff s1 s2)

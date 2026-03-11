
module QuickCheck where

import Data.List as L
import Data.Map.Strict as M
import Data.Tuple (swap)
import Data.Vector as V
import Debug.Trace
import Test.QuickCheck
import Test.Tasty.QuickCheck as QC
import Test.Tasty.TH

import Data.Paired.Foldable as DPF
import Data.Paired.Vector as DPV
import Math.TriangularNumbers

-- * Data.Paired.Vector

-- |

prop_vector_upperTri_On :: NonNegative Int -> Bool
prop_vector_upperTri_On (NonNegative k) = V.toList vs == ls
  where vs = snd $ upperTriVG OnDiag v
        ls = [ (a,b)
             | as@(a:_) <- L.init . L.tails $ V.toList v
             , b <- as
             ]
        v = V.enumFromTo 0 k

-- |

prop_vector_upperTri_No :: NonNegative Int -> Bool
prop_vector_upperTri_No (NonNegative k) = V.toList vs == ls
  where vs = snd $ upperTriVG NoDiag v
        ls = [ (a,b)
             | (a:as) <- L.init . L.tails $ V.toList v
             , b <- as
             ]
        v = V.enumFromTo 0 k

-- |

prop_vector_rectangular :: NonNegative Int -> NonNegative Int -> Bool
prop_vector_rectangular (NonNegative k) (NonNegative l) = V.toList vs == ls
  where vs = snd $ rectangularVG as bs
        ls = [ (a,b)
             | a <- V.toList as
             , b <- V.toList bs
             ]
        as = V.enumFromTo 0 k
        bs = V.enumFromTo 0 l



-- * Data.Paired.Foldable

-- | Generalized upper triangular elements. We want to enumerate all
-- elements, including those on the main diagonal.

prop_foldable_upperTri_On_All :: (NonNegative Int, Bool) -> Bool
prop_foldable_upperTri_On_All (NonNegative n, b)
  | chk       = True
  | otherwise = traceShow (ls,vs) False
  where Right (_,_,vs) = DPF.upperTri (if b then KnownSize n else UnknownSize) OnDiag All xs
        ls = [ ((a,b),(a,b))
             | as@(a:_) <- L.init . L.tails $ xs
             , b <- as
             ]
        xs = [ 0 .. n-1 ]
        chk = vs == ls

-- | Only a subset of elements, starting at @k@ (counting from 0) and
-- taking @s@ elements.

prop_foldable_upperTri_On_FromN :: (NonNegative Int, NonNegative Int, NonNegative Int, Bool) -> Bool
prop_foldable_upperTri_On_FromN (NonNegative n, NonNegative k, NonNegative s, b)
  | chk       = True
  | otherwise = traceShow (ls,vs) False
  where Right (_,_,vs) = DPF.upperTri (if b then KnownSize n else UnknownSize) OnDiag (FromN k s) xs
        ls = L.take s
           . L.drop k
           $ [ ((a,b),(a,b))
             | as@(a:_) <- L.init . L.tails $ xs
             , b <- as
             ]
        xs = [ 0 .. n-1 ]
        chk = vs == ls

prop_foldable_upperTri_No_All :: (NonNegative Int, Bool) -> Bool
prop_foldable_upperTri_No_All (NonNegative n, b)
  | chk       = True
  | otherwise = traceShow (ls,vs) False
  where Right (_,_,vs) = DPF.upperTri (if b then KnownSize n else UnknownSize) NoDiag All xs
        ls = [ ((a,b),(a,b))
             | (a:as) <- L.init . L.tails $ xs
             , b <- as
             ]
        xs = [ 0 .. n-1 ]
        chk = vs == ls

prop_foldable_upperTri_No_FromN :: (NonNegative Int, NonNegative Int, NonNegative Int, Bool) -> Bool
prop_foldable_upperTri_No_FromN (NonNegative n, NonNegative k, NonNegative s, b)
  | chk       = True
  | otherwise = traceShow (ls,vs) False
  where Right (_,_,vs) = DPF.upperTri (if b then KnownSize n else UnknownSize) NoDiag (FromN k s) xs
        ls = L.take s
           . L.drop k
           $ [ ((a,b),(a,b))
             | (a:as) <- L.init . L.tails $ xs
             , b <- as
             ]
        xs = [ 0 .. n-1 ]
        chk = vs == ls



-- * Math.TriangularNumbers

-- | Test that each index pair @(i,j)@ is assigned a unique linear index
-- @k@ given @0 <= i <= j <= n@.

prop_uniqueLinear :: NonNegative Int -> Bool
prop_uniqueLinear (NonNegative n) = M.null $ M.filter ((/=1) . L.length) mp
  where mp = M.fromListWith (L.++) [ (toLinear n (i,j), [(i,j)]) | i <- [0..n], j <- [i..n] ]

-- | Back and forth translation between paired and linear indices is
-- unique.

prop_BackForth :: NonNegative Int -> Bool
prop_BackForth (NonNegative n) = L.and xs
  where mb = M.fromList ls
        mf = M.fromList $ L.map swap ls
        ls = [ (toLinear n (i,j), (i,j)) | i <- [0..n], j <- [i..n] ]
        xs = [ (mb M.! k == (i,j)) && (mf M.! (i,j) == k) && fromLinear n k == (i,j)
             | (k,(i,j)) <- ls ]

--

-- | Check if both splitKeepEnd and simple tokenization provide the same
-- result.

--prop_splitKeepEndStrict :: String -> Small Int -> Small Int -> Bool
--prop_splitKeepEndStrict str' (Small k) (Small l)
--  | tt == ss  = True
--  | otherwise = traceShow ("ske",pat,str,k,l,tt,ss,ee) False
--  where str = BS.concat . L.replicate skeMult $ BS.pack str'
--        -- make a small pattern with a chance that it repeats
--        pat = BS.take (l `mod` 2 + 1) $ BS.drop (k `mod` 10) str
--        -- what ske thinks is a good split
--        (ss,ee,_) = ske pat str
--        -- manual splitting
--        tt = referenceByteStringTokenizer pat str

-- | Check if both splitKeepEnd and simple tokenization provide the same
-- result.

--prop_splitKeepEndLazy :: String -> Small Int -> Small Int -> Bool
--prop_splitKeepEndLazy str' (Small k) (Small l)
--  | tt == ll  = True
--  | otherwise = traceShow ("ske'",pat,str',str,strL,k,l,tt,ll,ee,rr) False
--  where str = BS.concat . L.replicate skeMult $ BS.pack str'
--        strL = BSL.fromChunks $ L.replicate skeMult $ BS.pack str'
--        -- make a small pattern with a chance that it repeats
--        pat = BS.take (l `mod` 2 + 1) $ BS.drop (k `mod` 10) str
--        -- what we get with the lazy version
--        (ll,ee,rr) = ske' pat strL
--        -- manual splitting
--        tt = referenceByteStringTokenizer pat str

-- The actual splitting system

--ske :: ByteString -> ByteString -> ([ByteString],[ByteString],[ByteString])
--ske pat str | BS.null pat || BS.null str = ([],[],[])
--ske pat str =
--  let parse = do
--        xs <- zoom (splitKeepEnd pat) PP.drawAll
--        case xs of
--          [] -> return $ Left []
--          xs -> return $ Right $ BS.concat xs
--      (a,(b,p)) = runIdentity . P.toListM' $ PP.parsed parse $ PP.yield str
--  in (a,b, fst . runIdentity . P.toListM' $ p)
--
--ske' :: ByteString -> BSL.ByteString -> ([ByteString],[ByteString],[ByteString])
--ske' pat _ | BS.null pat = ([],[],[])
--ske' pat str =
--  let parse = do
--        xs <- zoom (splitKeepEnd pat) PP.drawAll
--        case xs of
--          [] -> return $ Left []
--          xs -> return $ Right $ BS.concat xs
--      (a,(b,p)) = runIdentity . P.toListM' $ PP.parsed parse $ PB.fromLazy str
--  in (a,b, fst . runIdentity . P.toListM' $ p)

skeMult :: Int
skeMult = 1000



-- * Streaming tests.


testQuickCheck = $(testGroupGenerator)


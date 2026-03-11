
-- | Efficient enumeration of subsets of triangular elements. Given a list
-- @[1..n]@ we want to enumerate a subset @[(i,j)]@ of ordered pairs in
-- such a way that we only have to hold the elements necessary for this
-- subset in memory.

module Data.Paired.Foldable where

import Data.IntMap as IM
import Data.Foldable as F
import Data.List as L
import Control.Arrow ((***))
import Data.Vector as V
import Data.Vector.Generic as VG
import Debug.Trace (traceShow)
import Text.Printf

import Data.Paired.Common
import Math.TriangularNumbers



-- | Generalized upper triangular elements. Given a list of elements
-- @[e_1,...,e_k]@, we want to return pairs @(e_i,e_j)@ such that we have
-- all ordered pairs with @i<j@ (if @NoDiag@onal elements), or @i<=j@ (if
-- @OnDiag@onal elements).
--
-- @upperTri@ will force the spine of @t a@ but is consumed linearly with
-- a strict @Data.Foldable.foldl'@. Internally we keep a @Data.IntMap@ of
-- the retained elements.
--
-- This is important if the @Enumerate@ type is set to @FromN k n@. We
-- start at the @k@th element, and produce @n@ elements.
--
-- TODO compare @IntMap@ and @HashMap@.
--
-- TODO inRange is broken.

upperTri
  :: (Foldable t)
  => SizeHint
  -- ^ If the size of @t a@ is known beforehand, give the appropriate
  -- @KnownSize n@, otherwise give @UnknownSize@. Using @UnknownSize@ will
  -- force the complete spine of @t a@.
  -> OnDiag
  -- ^ The enumeration will include the pairs on the main diagonal with
  -- @OnDiag@, meaning @(i,i)@ will be included for all @i@. Otherwise,
  -- @NoDiag@ will exclude these elements.
  -> Enumerate
  -- ^ Either enumerate @All@ elements or enumerate the @s@ elements
  -- starting at @k@ with @FromN k s@.
  -> t a
  -- ^ The foldable data structure to enumerate over.
  -> Either String (IntMap a, Int, [((Int,Int),(a,a))])
  -- ^ If there is any error then return @Left errorMsg@. Otherwise we have
  -- @Right (imap, numElems, list)@. The @imap@ structure holds the subset
  -- of elements with which we actually generate elements. @numElems@ is
  -- the total number of elements that will be generated. This is
  -- calculated without touch @list@. Finally, @list@ is the lazy list of
  -- elements to be generated.
upperTri sz d e xs
  | szLen /= readLen = Left $ printf "Expected SizeHint %d elements, but processed only %d elements!" szLen readLen
  | otherwise        = Right (imp, numElems, ys)
  where ys   = case e of {All -> id ; FromN _ s -> L.take s}
             . L.unfoldr go $ initEnum e d
        -- how many elements we will emit depends on enumeration and on
        -- diagonal element counting
        numElems
          | All <- e       = allSize
          | FromN s k <- e = if s+k > allSize then max 0 (allSize - s) else k
        -- The length of the input. With a given size hint, @xs :: t a@
        -- will only be touched once.
#if MIN_VERSION_base(4,8,0)
        szLen = case sz of { UnknownSize -> F.length xs ; KnownSize z -> z }
#else
        szLen = case sz of { UnknownSize -> L.length . F.toList $ xs ; KnownSize z -> z }
#endif
        szLn' = case d of { OnDiag -> szLen - 1 ; NoDiag -> szLen - 2 }
        -- Construct an intmap @imp@ of all elements in the accepted range.
        -- At the same time, return the length or size of the foldable
        -- container we gave as input. @xs@ is touched only once and can
        -- be efficiently consumed.
        (!imp,!readLen) = F.foldl' (\(!i,!l) x -> (if inRange l then IM.insert l x i else i,l+1)) (IM.empty, 0) xs
        allSize = szLen * (szLen + if d == OnDiag then 1 else -1) `div` 2
        -- we need three ranges. @cMin@ and @cMax@ are the range for the
        -- slow-moving first element in the tuple. @rMin@ and @rMax@ are
        -- the first and last element of the range starting at @cMin@ (we
        -- can actually start at @cMax@ but it doesn't matter).
        -- Finally, @lMin@ and @lMax@ are the range to the left of @cMin@.
        (lMin,lMax,cMin,cMax,rMin,rMax) = case e of
          All -> (0, szLen-1, 0, szLen-1, 0, szLen-1)
          FromN s k ->
            let (cmin,rmin) = fromLinear szLn' s
                (cmax,_   ) = fromLinear szLn' (s+k)
                rmax = rmin+k -- if this is @>= len@ we are safe anyway.
                lmin = if rmin+k >= szLen then 0 else cmin
                lmax = if rmin+k >= szLen then lmin + toLinear szLn' (cmin+1,cmin+1+rmin+k-szLn') else cmax
            in  (lmin, lmax, cmin, cmax, rmin, rmax)
        -- Determine if an element at linear index @z@ is in the range to
        -- be consumed.
        inRange z =  lMin <= z && z <= lMax
                  || cMin <= z && z <= cMax
                  || rMin <= z && z <= rMax
        -- index into the generated vector @xs@ when generating elements
        -- via @go@
        go (k,l)
          | k >= szLen  = Nothing
          | l >= szLen  = go (k+1,k+1 + if d == OnDiag then 0 else 1)
          | otherwise = Just (((k,l),(imp IM.! k, imp IM.! l)), (k,l+1))
        -- Initialize the enumeration at the correct pair @(i,j)@. From
        -- then on we can @take@ the correct number of elements, or stream
        -- all of them.
        initEnum All OnDiag = (0,0)
        initEnum All NoDiag = (0,1)
        initEnum (FromN s k) OnDiag
          | s >= allSize = (szLen,szLen)
          | otherwise    = fromLinear szLn' s
        initEnum (FromN s k) NoDiag
          | s >= allSize = (szLen,szLen)
          | otherwise    = id *** (+1) $ fromLinear szLn' s


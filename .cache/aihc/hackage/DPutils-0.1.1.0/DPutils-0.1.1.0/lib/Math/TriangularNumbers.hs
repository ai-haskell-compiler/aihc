
-- | Triangular numbers and various helper functions.
--
-- Main use is for linearization of triangular array indexing.
-- 
-- Triangular numbers:
-- @
-- T_n = Σ_{k=1)^n k = 1 + 2 + 3 + ... + n =
--
-- n * (n+1) / 2 = (n+1) `choose` 2
-- @
--
--

module Math.TriangularNumbers where



-- | Triangular numbers.
--
-- https://oeis.org/A000217

triangularNumber :: Int -> Int
triangularNumber x = (x * (x+1)) `quot` 2
{-# INLINE triangularNumber #-}

-- | Size of an upper triangle starting at 'i' and ending at 'j'. "(0,N)" what
-- be the normal thing to use.

linearizeUppertri :: (Int,Int) -> Int
linearizeUppertri (i,j) = triangularNumber $ j-i+1
{-# INLINE linearizeUppertri #-}

-- | Subword indexing. Given the longest subword and the current subword,
-- calculate a linear index "[0,..]". "(l,n)" in this case means "l"ower bound,
-- length "n". And "(i,j)" is the normal index.
--
-- @
-- 0 1 2 3    <- j = ...
--
-- 0 1 2 3    i=0
-- _ 4 5 6    i=1
-- _ _ 7 8    i=2
--       9    i=3
--
-- i=2, j=3  -> (4+1) * i - tri i + j
--
-- _
-- _ _  the triangular number to subtract.
-- @

toLinear :: Int -> (Int,Int) -> Int
toLinear n (i,j) = adr n (i,j)
  where
    adr n (i,j) = (n+1)*i - triangularNumber i + j
    {-# Inline adr #-}
{-# INLINE toLinear #-}



-- | Linear index to paired.
--
-- We have indices in @[0,N]@, and linear index @k@.
--
-- @
-- (N+1)*i - (i*(i+1)/2) + j == K
-- @

fromLinear :: Int -> Int -> (Int,Int)
fromLinear n' k' = (i,j)
  where ll = (2*n+1) / 2
        rr = sqrt $ ((2*(n+1)+1) / 2)^2 - 2*k
        n  = fromIntegral n'
        k  = fromIntegral k'
        i  = floor $ ll - rr + 1
        j  = k' - toLinear n' (i,0)
{-# Inline fromLinear #-}


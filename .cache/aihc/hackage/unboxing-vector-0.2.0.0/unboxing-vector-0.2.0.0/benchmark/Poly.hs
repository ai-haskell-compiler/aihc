{-# LANGUAGE BangPatterns #-}
module Poly where
import Prelude hiding (gcd, div, mod, divMod, const)
import Control.Monad
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxing -- for writing SPECIALIZE instance pragma
import qualified Data.Vector -- for writing SPECIALIZE instance pragma

infixl 7 `div`, `mod`

-- univariate polynomial, coefficients in ascending order
newtype Poly vect a = Poly (vect a) deriving (Eq,Show)

zero :: (G.Vector vect a) => Poly vect a
zero = Poly G.empty
{-# INLINE zero #-}

isZero :: (G.Vector vect a) => Poly vect a -> Bool
isZero (Poly xs) = G.null xs
{-# INLINE isZero #-}

const :: (Eq a, Num a, G.Vector vect a) => a -> Poly vect a
const 0 = zero
const a = Poly (G.singleton a)
{-# INLINE const #-}

x :: (Num a, Eq a, G.Vector vect a) => Poly vect a
x = Poly (G.fromList [0, 1])
{-# INLINE x #-}

fromCoeffAsc :: (Eq a, Num a, G.Vector vect a) => vect a -> Poly vect a
fromCoeffAsc xs
  | G.null xs = Poly G.empty
  | G.last xs == 0 = fromCoeffAsc (G.init xs)
  | otherwise = Poly xs
{-# INLINE fromCoeffAsc #-}

coeffAsc :: Poly vect a -> vect a
coeffAsc (Poly xs) = xs
{-# INLINE coeffAsc #-}

addPoly :: (Eq a, Num a, G.Vector vect a) => Poly vect a -> Poly vect a -> Poly vect a
addPoly (Poly xs) (Poly ys)
  | n < m = Poly $ G.create $ do
      v <- GM.new m
      forM_ [0..n-1] $ \i -> GM.write v i ((xs G.! i) + (ys G.! i))
      forM_ [n..m-1] $ \i -> GM.write v i (ys G.! i)
      return v
  | m < n = Poly $ G.create $ do
      v <- GM.new n
      forM_ [0..m-1] $ \i -> GM.write v i ((xs G.! i) + (ys G.! i))
      forM_ [m..n-1] $ \i -> GM.write v i (xs G.! i)
      return v
  | otherwise = fromCoeffAsc $ G.zipWith (+) xs ys
  where n = G.length xs
        m = G.length ys
{-# INLINABLE addPoly #-}

subPoly :: (Eq a, Num a, G.Vector vect a) => Poly vect a -> Poly vect a -> Poly vect a
subPoly (Poly xs) (Poly ys)
  | n < m = Poly $ G.create $ do
      v <- GM.new m
      forM_ [0..n-1] $ \i -> GM.write v i ((xs G.! i) - (ys G.! i))
      forM_ [n..m-1] $ \i -> GM.write v i (negate (ys G.! i))
      return v
  | m < n = Poly $ G.create $ do
      v <- GM.new n
      forM_ [0..m-1] $ \i -> GM.write v i ((xs G.! i) - (ys G.! i))
      forM_ [m..n-1] $ \i -> GM.write v i (xs G.! i)
      return v
  | otherwise = fromCoeffAsc $ G.zipWith (-) xs ys
  where n = G.length xs
        m = G.length ys
{-# INLINABLE subPoly #-}

-- multiplication: naive method
mulPoly :: (Eq a, Num a, G.Vector vect a) => Poly vect a -> Poly vect a -> Poly vect a
mulPoly (Poly xs) (Poly ys)
  | n == 0 || m == 0 = zero
  | otherwise = Poly $ G.generate (n + m - 1) (\i -> sum [(xs G.! j) * (ys G.! (i - j)) | j <- [max 0 (i - m + 1)..min i (n-1)]])
  where n = G.length xs
        m = G.length ys
{-# INLINABLE mulPoly #-}

instance (Eq a, Num a, G.Vector vect a) => Num (Poly vect a) where
  negate (Poly xs) = Poly $ G.map negate xs
  {-# INLINE negate #-}

  (+) = addPoly
  {-# INLINE (+) #-}

  (-) = subPoly
  {-# INLINE (-) #-}

  (*) = mulPoly
  {-# INLINE (*) #-}

  fromInteger n = const $ fromInteger n
  {-# INLINE fromInteger #-}

  abs = undefined
  signum = undefined

  {-# SPECIALIZE instance (Eq a, Num a) => Num (Poly Data.Vector.Vector a) #-}
  {-# SPECIALIZE instance (Eq a, Num a, Data.Vector.Unboxing.Unboxable a) => Num (Poly Data.Vector.Unboxing.Vector a) #-}

degree :: (G.Vector vect a) => Poly vect a -> Maybe Int
degree (Poly xs) = case G.length xs - 1 of
  -1 -> Nothing
  n -> Just n
{-# INLINE degree #-}

degree' :: (G.Vector vect a) => Poly vect a -> Int
degree' (Poly xs) = case G.length xs of
  0 -> error "degree': zero polynomial"
  n -> n - 1
{-# INLINE degree' #-}

leadingCoefficient :: (Num a, G.Vector vect a) => Poly vect a -> a
leadingCoefficient (Poly xs)
  | G.null xs = 0
  | otherwise = G.last xs
{-# INLINE leadingCoefficient #-}

toMonic :: (Fractional a, G.Vector vect a) => Poly vect a -> Poly vect a
toMonic f@(Poly xs)
  | G.null xs = zero
  | otherwise = Poly $ G.map (* recip (leadingCoefficient f)) xs
{-# INLINE toMonic #-}

scale :: (Eq a, Num a, G.Vector vect a) => a -> Poly vect a -> Poly vect a
scale a (Poly xs)
  | a == 0 = zero
  | otherwise = Poly $ G.map (* a) xs
{-# INLINE scale #-}

divMod :: (Eq a, Fractional a, G.Vector vect a) => Poly vect a -> Poly vect a -> (Poly vect a, Poly vect a)
divMod f g
  | isZero g = error "divMod: divide by zero"
  | degree f < degree g = (zero, f)
  | otherwise = loop zero (scale (recip b) f)
  where
    g' = toMonic g
    b = leadingCoefficient g
    -- invariant: f == q * g + scale b p
    loop q !p | degree p < degree g = (q, scale b p)
              | otherwise = let !q' = Poly (G.drop (degree' g) (coeffAsc p))
                            in loop (q + q') (p - q' * g')
{-# INLINABLE divMod #-}

div, mod :: (Eq a, Fractional a, G.Vector vect a) => Poly vect a -> Poly vect a -> Poly vect a
div f g = fst (divMod f g)
mod f g = snd (divMod f g)
{-# INLINE div #-}
{-# INLINE mod #-}

-- | GCD with naive Euclidean algorithm
gcd :: (Eq a, Fractional a, G.Vector vect a) => Poly vect a -> Poly vect a -> Poly vect a
gcd f g | isZero g = f
        | otherwise = gcd g (f `mod` g)
{-# INLINE gcd #-}

powMod :: (Eq a, Fractional a, G.Vector vect a) => Poly vect a -> Int -> Poly vect a -> Poly vect a
powMod _a 0 _modulo = 1
powMod a n modulo = loop (n-1) a a
  where loop 0 !_ !acc = acc
        loop 1 !m !acc = m * acc `mod` modulo
        loop i !m !acc
          | even i = loop (i `quot` 2) (m * m `mod` modulo) acc
          | otherwise = loop (i `quot` 2) (m * m `mod` modulo) (m * acc `mod` modulo)
{-# INLINABLE powMod #-}

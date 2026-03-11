{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}

module Data.Lexicographic.Generic.Internal where

import GHC.Generics

class GBounded ra where
  gminBound :: ra x
  gmaxBound :: ra x

instance GBounded U1 where
  gminBound = U1
  gmaxBound = U1

instance Bounded a => GBounded (K1 _m a) where
  gminBound = K1 minBound
  gmaxBound = K1 maxBound

instance GBounded ra => GBounded (M1 _d _m ra) where
  gminBound = M1 gminBound
  gmaxBound = M1 gmaxBound

instance (GBounded ra, GBounded rb) => GBounded (ra :+: rb) where
  gminBound = L1 gminBound
  gmaxBound = R1 gmaxBound

instance (GBounded ra, GBounded rb) => GBounded (ra :*: rb) where
  gminBound = gminBound :*: gminBound
  gmaxBound = gmaxBound :*: gmaxBound

class GEnum ra where
  gfromEnum :: ra x -> Int
  gtoEnum :: Int -> ra x

instance GEnum U1 where
  gfromEnum U1 = 0
  gtoEnum 0 = U1
  gtoEnum _ = error "Import.GEnum.U1.gtoEnum: bad argument"

instance Enum a => GEnum (K1 _m a) where
  gfromEnum (K1 x) = fromEnum x
  gtoEnum n = K1 (toEnum n)

instance GEnum ra => GEnum (M1 _d _m ra) where
  gfromEnum (M1 x) = gfromEnum x
  gtoEnum n = M1 (gtoEnum n)

instance (GBounded ra, GEnum ra, GBounded rb, GEnum rb) => GEnum (ra :+: rb) where
  gfromEnum (L1 x) = gfromEnum x
  gfromEnum (R1 y) = gfromEnum (gmaxBound :: ra x) + 1 + gfromEnum y

  gtoEnum n
    | n <= gfromEnum (gmaxBound :: ra x) = L1 (gtoEnum n :: ra x)
    | otherwise = R1 (gtoEnum (n - 1 - gfromEnum (gmaxBound :: ra x)))

instance (GBoundEnum ra, GBoundEnum rb) => GEnum (ra :*: rb) where
  gfromEnum (x :*: y) = gfromEnum x * (gcardinality ([] :: [rb x])) + gfromEnum y
  gtoEnum n = case n `divMod` gcardinality ([] :: [rb x]) of
    (n1,n2) -> (gtoEnum n1 :*: gtoEnum n2)

class (GBounded ra, GEnum ra) => GBoundEnum ra where
  gcardinality :: p (ra x) -> Int

instance GBoundEnum U1 where
  gcardinality _ = 1

instance (Bounded a, Enum a) => GBoundEnum (K1 _m a) where
  gcardinality _ = length [minBound .. (maxBound :: a)]

instance GBoundEnum ra => GBoundEnum (M1 _d _m ra) where
  gcardinality _ = gcardinality ([] :: [ra x])

instance (GBoundEnum ra, GBoundEnum rb) => GBoundEnum (ra :+: rb) where
  gcardinality _ = gcardinality ([] :: [ra x]) + gcardinality ([] :: [rb x])

instance (GBoundEnum ra, GBoundEnum rb) => GBoundEnum (ra :*: rb) where
  gcardinality _ = gcardinality ([] :: [ra x]) * gcardinality ([] :: [rb x])

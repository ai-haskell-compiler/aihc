{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | Functions for manipulating shape. The module tends to supply equivalent functionality at type-level and value-level with functions of the same name (except for capitalization).
module Harpie.Shape
  ( -- * Type-level Nat
    SNat,
    pattern SNat,
    valueOf,

    -- * Type-level [Nat]
    SNats,
    pattern SNats,
    fromSNats,
    KnownNats (..),
    natVals,
    withKnownNats,
    SomeNats,
    someNatVals,
    withSomeSNats,

    -- * Shape
    valuesOf,
    rankOf,
    sizeOf,
    Fin (..),
    fin,
    safeFin,
    Fins (..),
    fins,
    safeFins,

    -- * Shape Operators at value- and type- level.
    rank,
    Rank,
    range,
    Range,
    rerank,
    Rerank,
    dimsOf,
    DimsOf,
    endDimsOf,
    EndDimsOf,
    size,
    Size,
    flatten,
    shapen,
    asSingleton,
    AsSingleton,
    asScalar,
    AsScalar,
    isSubset,
    IsSubset,
    exceptDims,
    ExceptDims,
    reorder,
    Reorder,
    ReorderOk,
    squeeze,
    Squeeze,

    -- * Primitives
    Min,
    Max,
    minimum,
    Minimum,

    -- * Position
    isFin,
    IsFin,
    isFins,
    IsFins,
    isDim,
    IsDim,
    isDims,
    IsDims,
    lastPos,
    LastPos,
    minDim,
    MinDim,

    -- * combinators
    EnumFromTo,
    Foldl',

    -- * single dimension
    GetIndex,
    SetIndex,
    getDim,
    GetDim,
    modifyDim,
    ModifyDim,
    incAt,
    IncAt,
    decAt,
    DecAt,
    setDim,
    SetDim,
    takeDim,
    TakeDim,
    dropDim,
    DropDim,
    deleteDim,
    DeleteDim,
    insertDim,
    InsertDim,
    InsertOk,
    SliceOk,
    SlicesOk,
    concatenate,
    Concatenate,
    ConcatenateOk,

    -- * multiple dimension
    getDims,
    GetDims,
    getLastPositions,
    GetLastPositions,
    modifyDims,
    insertDims,
    InsertDims,
    preDeletePositions,
    PreDeletePositions,
    preInsertPositions,
    PreInsertPositions,
    setDims,
    SetDims,
    deleteDims,
    DeleteDims,
    dropDims,
    DropDims,
    concatDims,
    ConcatDims,

    -- * value-only operations
    unconcatDimsIndex,
    reverseIndex,
    rotate,
    rotateIndex,
    rotatesIndex,
    isDiag,

    -- * windowed
    expandWindows,
    ExpandWindows,
    indexWindows,
    dimWindows,
    DimWindows,

    -- * Fcf re-exports
    Eval,
    type (++),
  )
where

import Data.Bool
import Data.Foldable hiding (minimum)
import Data.Function
import Data.List qualified as List
import Data.Maybe
import Data.Proxy
import Data.Type.Bool hiding (Not)
import Data.Type.Equality
import Data.Type.Ord hiding (Max, Min)
import Fcf hiding (type (&&), type (+), type (++), type (-), type (<), type (>), type (||))
import Fcf qualified
import Fcf.Class.Foldable
import Fcf.Data.List
import GHC.Exts
import GHC.TypeLits (ErrorMessage (..))
import GHC.TypeLits qualified as L
import GHC.TypeNats
import Prelude as P hiding (minimum)

{-# ANN module ("doctest-parallel: --no-implicit-module-import" :: String) #-}

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeFamilies
-- >>> import Prelude
-- >>> import Fcf
-- >>> import GHC.Exts ()
-- >>> import Harpie.Shape as S

-- | Get the value of a type level Nat.
-- Use with explicit type application
--
-- >>> valueOf @42
-- 42
valueOf :: forall n. (KnownNat n) => Int
valueOf = fromIntegral $ fromSNat (SNat @n)
{-# INLINE valueOf #-}

type role SNats nominal

-- | A value-level witness for a type-level list of natural numbers.
--
-- Obtain an SNats value using:
--
-- - The natsSing method of KnownNats
-- - The SNats pattern
-- - The withSomeSNats function
--
-- >>> :t SNats @[2,3,4]
-- SNats @[2,3,4] :: KnownNats [2, 3, 4] => SNats [2, 3, 4]
-- >>> SNats @[2,3,4]
-- SNats @[2, 3, 4]
newtype SNats (ns :: [Nat]) = UnsafeSNats [Nat]

instance Eq (SNats ns) where
  _ == _ = True

instance Ord (SNats ns) where
  compare _ _ = EQ

-- | Matches GHC printing quirks.
instance Show (SNats ns) where
  show (UnsafeSNats s) = "SNats @" <> bool "" "'" (length s < 2) <> "[" <> mconcat (List.intersperse ", " (show <$> s)) <> "]"

-- | A explicitly bidirectional pattern synonym relating an 'SNats' to a 'KnownNats' constraint.
--
-- As an expression: Constructs an explicit 'SNats' ns value from an implicit 'KnownNats' ns constraint:
--
-- > SNat @n :: KnownNat n => SNat n
--
-- As a pattern: Matches on an explicit SNats n value bringing an implicit KnownNats n constraint into scope:
--
-- > f :: SNats ns -> ..
-- > f SNat = {- KnownNats ns in scope -}
--
-- or, if you need to both bring the KnownNats into scope and reuse the SNats input:
--
-- > f (SNats :: SNats s) = g (SNats @s)
pattern SNats :: forall ns. () => (KnownNats ns) => SNats ns
pattern SNats <- (knownNatsInstance -> KnownNatsInstance)
  where
    SNats = natsSing

{-# COMPLETE SNats #-}

-- | Return the value-level list of naturals in an SNats ns value.
--
-- >>> fromSNats (SNats @[2,3,4])
-- [2,3,4]
fromSNats :: SNats s -> [Nat]
fromSNats (UnsafeSNats s) = s

-- An internal data type that is only used for defining the SNat pattern
-- synonym.
data KnownNatsInstance (ns :: [Nat]) where
  KnownNatsInstance :: (KnownNats ns) => KnownNatsInstance ns

-- An internal function that is only used for defining the SNat pattern
-- synonym.
knownNatsInstance :: SNats ns -> KnownNatsInstance ns
knownNatsInstance dims = withKnownNats dims KnownNatsInstance

-- | Reflect a list of naturals.
--
-- >>> natsSing @'[2]
-- SNats @'[2]
class KnownNats (ns :: [Nat]) where
  natsSing :: SNats ns

instance KnownNats '[] where
  natsSing = UnsafeSNats []

instance (KnownNat n, KnownNats s) => KnownNats (n ': s) where
  natsSing = UnsafeSNats (fromSNat (SNat :: SNat n) : fromSNats (SNats :: SNats s))

-- | Obtain a value-level list of naturals from a type-level proxy
--
-- >>> natVals (SNats @[2,3,4])
-- [2,3,4]
natVals :: forall ns proxy. (KnownNats ns) => proxy ns -> [Nat]
natVals _ = case natsSing :: SNats ns of
  UnsafeSNats xs -> xs

-- | Convert an explicit SNats ns value into an implicit KnownNats ns constraint.
withKnownNats ::
  forall ns rep (r :: TYPE rep).
  SNats ns -> ((KnownNats ns) => r) -> r
withKnownNats = withDict @(KnownNats ns)

-- | Convert a list of naturals into an SNats ns value, where ns is a fresh type-level list of naturals.
withSomeSNats ::
  forall rep (r :: TYPE rep).
  [Nat] -> (forall s. SNats s -> r) -> r
withSomeSNats s k = k (UnsafeSNats s)
{-# NOINLINE withSomeSNats #-}

-- | An unknown type-level list of naturals.
data SomeNats = forall s. (KnownNats s) => SomeNats (Proxy s)

-- | Promote a list of naturals to unknown type-level
someNatVals :: [Nat] -> SomeNats
someNatVals s =
  withSomeSNats
    s
    ( \(sn :: SNats s) ->
        withKnownNats sn (SomeNats @s Proxy)
    )

-- * shape primitives

-- | The value of a 'KnownNats'.
--
-- >>> valuesOf @[2,3,4]
-- [2,3,4]
valuesOf :: forall s. (KnownNats s) => [Int]
valuesOf = fmap fromIntegral (fromSNats (SNats :: SNats s))
{-# INLINE valuesOf #-}

-- | The rank (or length) of a KnownNats.
--
-- >>> rankOf @[2,3,4]
-- 3
rankOf :: forall s. (KnownNats s) => Int
rankOf = length (valuesOf @s)
{-# INLINE rankOf #-}

-- | The size (or product) of a KnownNats.
--
-- >>> sizeOf @[2,3,4]
-- 24
sizeOf :: forall s. (KnownNats s) => Int
sizeOf = product (valuesOf @s)
{-# INLINE sizeOf #-}

-- | Fin most often represents a (finite) zero-based index for a single dimension (of a multi-dimensioned hyper-rectangular array).
type role Fin nominal

newtype Fin s
  = UnsafeFin
  { fromFin :: Int
  }
  deriving stock (Eq, Ord)

instance Show (Fin n) where
  show (UnsafeFin x) = show x

-- | Construct a Fin.
--
-- Errors on out-of-bounds
--
-- >>> fin @2 1
-- 1
--
-- >>> fin @2 2
-- *** Exception: value outside bounds
-- ...
fin :: forall n. (KnownNat n) => Int -> Fin n
fin x = fromMaybe (error "value outside bounds") (safeFin x)

-- | Construct a Fin safely.
--
-- >>> safeFin 1 :: Maybe (Fin 2)
-- Just 1
--
-- >>> safeFin 2 :: Maybe (Fin 2)
-- Nothing
safeFin :: forall n. (KnownNat n) => Int -> Maybe (Fin n)
safeFin x = bool Nothing (Just (UnsafeFin x)) (x >= 0 && x < valueOf @n)

-- | Fins most often represents (finite) indexes for multiple dimensions (of a multi-dimensioned hyper-rectangular array).
type role Fins nominal

newtype Fins s
  = UnsafeFins
  { fromFins :: [Int]
  }
  deriving stock (Eq, Ord, Functor)

instance Show (Fins n) where
  show (UnsafeFins x) = show x

-- | Construct a Fins.
--
-- Errors on out-of-bounds
--
-- >>> fins @[2,3,4] [1,2,3]
-- [1,2,3]
--
-- >>> fins @[2,3,4] [1,2,5]
-- *** Exception: value outside bounds
-- ...
fins :: forall s. (KnownNats s) => [Int] -> Fins s
fins x = fromMaybe (error "value outside bounds") (safeFins x)

-- | Construct a Fins safely.
--
-- >>> safeFins [1,2,3] :: Maybe (Fins [2,3,4])
-- Just [1,2,3]
--
-- >>> safeFins [2] :: Maybe (Fins '[2])
-- Nothing
safeFins :: forall s. (KnownNats s) => [Int] -> Maybe (Fins s)
safeFins xs = bool Nothing (Just (UnsafeFins xs)) (isFins xs (valuesOf @s))

-- | Number of dimensions
--
-- >>> rank @Int [2,3,4]
-- 3
rank :: [a] -> Int
rank = length
{-# INLINE rank #-}

-- | Number of dimensions
--
-- >>> :k! Eval (Rank [2,3,4])
-- ...
-- = 3
data Rank :: [a] -> Exp Natural

type instance
  Eval (Rank xs) =
    Eval (Length xs)

-- | Enumerate a range of rank n
--
-- >>> range 0
-- []
--
-- >>> range 3
-- [0,1,2]
range :: Int -> [Int]
range n = [0 .. (n - 1)]

-- | Enumerate a range of rank n
--
-- >>> :k! Eval (Range 0)
-- ...
-- = '[]
--
-- >>> :k! Eval (Range 3)
-- ...
-- = [0, 1, 2]
data Range :: Nat -> Exp [Nat]

type instance
  Eval (Range x) =
    If (x == 0) '[] (Eval (EnumFromTo 0 (Eval ((Fcf.-) x 1))))

-- | Create a new rank by adding ones to the left, if the new rank is greater, or combining dimensions (from left to right) into rows, if the new rank is lower.
--
-- >>> rerank 4 [2,3,4]
-- [1,2,3,4]
-- >>> rerank 2 [2,3,4]
-- [6,4]
rerank :: Int -> [Int] -> [Int]
rerank r xs =
  replicate (r - r') 1
    <> bool [] [product (take (r' - r + 1) xs)] (r <= r')
    <> drop (r' - r + 1) xs
  where
    r' = rank xs

-- | Create a new rank by adding ones to the left, if the new rank is greater, or combining dimensions (from left to right) into rows, if the new rank is lower.
--
-- >>> :k! Eval (Rerank 4 [2,3,4])
-- ...
-- = [1, 2, 3, 4]
-- >>> :k! Eval (Rerank 2 [2,3,4])
-- ...
-- = [6, 4]
data Rerank :: Nat -> [Nat] -> Exp [Nat]

type instance
  Eval (Rerank r xs) =
    If
      (Eval ((Fcf.>) r (Eval (Rank xs))))
      (Eval (Eval (Replicate (Eval ((Fcf.-) r (Eval (Rank xs)))) 1) ++ xs))
      ( Eval
          ( '[Eval (Size (Eval (Take (Eval ((Fcf.+) (Eval ((Fcf.-) (Eval (Rank xs)) r)) 1)) xs)))]
              ++ Eval (Drop (Eval ((Fcf.+) (Eval ((Fcf.-) (Eval (Rank xs)) r)) 1)) xs)
          )
      )

-- | Enumerate the dimensions of a shape.
--
-- dimsOf [2,3,4]
-- [0,1,2]
dimsOf :: [Int] -> [Int]
dimsOf s = range (rank s)

-- | Enumerate the dimensions of a shape.
--
-- >>> :k! Eval (DimsOf [2,3,4])
-- ...
-- = [0, 1, 2]
data DimsOf :: [Nat] -> Exp [Nat]

type instance
  Eval (DimsOf xs) =
    Eval (Range =<< Rank xs)

-- | Enumerate the final dimensions of a shape.
--
-- >>> endDimsOf [1,0] [2,3,4]
-- [2,1]
endDimsOf :: [Int] -> [Int] -> [Int]
endDimsOf xs s = take (rank xs) (List.reverse (dimsOf s))

-- | Enumerate the final dimensions of a shape.
--
-- >>> :k! Eval (EndDimsOf [1,0] [2,3,4])
-- ...
-- = [2, 1]
data EndDimsOf :: [Nat] -> [Nat] -> Exp [Nat]

type instance
  Eval (EndDimsOf xs s) =
    Eval (LiftM2 Take (Rank xs) (Reverse =<< DimsOf s))

-- | Total number of elements (if the list is the shape of a hyper-rectangular array).
--
-- >>> size [2,3,4]
-- 24
size :: [Int] -> Int
size [] = 1
size [x] = x
size xs = P.product xs
{-# INLINE size #-}

-- | Total number of elements (if the list is the shape of a hyper-rectangular array).
--
-- >>> :k! (Eval (Size [2,3,4]))
-- ...
-- = 24
data Size :: [Nat] -> Exp Nat

type instance Eval (Size xs) = Eval (Foldr (Fcf.*) 1 xs)

-- | Convert from a n-dimensional shape list index to a flat index, which, technically is the lexicographic position of the position in a row-major array.
--
-- >>> flatten [2,3,4] [1,1,1]
-- 17
--
-- >>> flatten [] [1,1,1]
-- 0
flatten :: [Int] -> [Int] -> Int
flatten [] _ = 0
flatten _ [x'] = x'
flatten ns xs = sum $ zipWith (*) xs (drop 1 $ scanr (*) 1 ns)
{-# INLINE flatten #-}

-- | Convert from a flat index to a shape index.
--
-- >>> shapen [2,3,4] 17
-- [1,1,1]
shapen :: [Int] -> Int -> [Int]
shapen [] _ = []
shapen [_] x' = [x']
shapen [_, y] x' = let (i, j) = divMod x' y in [i, j]
shapen ns x =
  fst $
    foldr
      ( \a (acc, r) ->
          let (d, m) = divMod r a
           in (m : acc, d)
      )
      ([], x)
      ns
{-# INLINE shapen #-}

-- | Convert a scalar to a dimensioned shape
--
-- >>> asSingleton []
-- [1]
-- >>> asSingleton [2,3,4]
-- [2,3,4]
asSingleton :: [Int] -> [Int]
asSingleton [] = [1]
asSingleton x = x

-- | Convert a scalar to a dimensioned shape
-- >>> :k! Eval (AsSingleton '[])
-- ...
-- = '[1]
-- >>> :k! Eval (AsSingleton [2,3,4])
-- ...
-- = [2, 3, 4]
data AsSingleton :: [Nat] -> Exp [Nat]

type instance
  Eval (AsSingleton xs) =
    If (xs == '[]) '[1] xs

-- | Convert a (potentially) [1] dimensioned shape to a scalar shape
--
-- >>> asScalar [1]
-- []
-- >>> asScalar [2,3,4]
-- [2,3,4]
asScalar :: [Int] -> [Int]
asScalar [1] = []
asScalar x = x

-- | Convert a (potentially) [1] dimensioned shape to a scalar shape
-- >>> :k! Eval (AsScalar '[1])
-- ...
-- = '[]
-- >>> :k! Eval (AsScalar [2,3,4])
-- ...
-- = [2, 3, 4]
data AsScalar :: [Nat] -> Exp [Nat]

type instance
  Eval (AsScalar xs) =
    If (xs == '[1]) '[] xs

lte :: [Int] -> [Int] -> Bool
lte xs ys =
  and (zipWith (<=) xs ys)
    && rank xs == rank ys

data LTE :: [Nat] -> [Nat] -> Exp Bool

type instance
  Eval (LTE xs ys) =
    Eval
      ( LiftM2
          (Fcf.&&)
          (And =<< ZipWith (Fcf.<=) xs ys)
          (LiftM2 TyEq (Rank xs) (Rank ys))
      )

-- | Check if a shape is a subset (<=) another shape after reranking.
--
-- >>> isSubset [2,3,4] [2,3,4]
-- True
--
-- >>> isSubset [1,2] [2,3,4]
-- True
--
-- >>> isSubset [2,1] [1]
-- False
isSubset :: [Int] -> [Int] -> Bool
isSubset xs ys = lte (rerank (rank ys) xs) ys

-- | Check if a shape is a subset (<=) another shape after reranking.
--
-- >>> :k! Eval (IsSubset [2,3,4] [2,3,4])
-- ...
-- = True
--
-- >>> :k! Eval (IsSubset [1,2] [2,3,4])
-- ...
-- = True
--
-- >>> :k! Eval (IsSubset [2,1] '[1])
-- ...
-- = False
data IsSubset :: [Nat] -> [Nat] -> Exp Bool

type instance
  Eval (IsSubset xs ys) =
    Eval (LTE (Eval (Rerank (Eval (Rank ys)) xs)) ys)

-- | Compute dimensions for a shape other than the supplied dimensions.
--
-- >>> exceptDims [1,2] [2,3,4]
-- [0]
exceptDims :: [Int] -> [Int] -> [Int]
exceptDims ds s = deleteDims ds [0 .. (rank s - 1)]

-- | Compute dimensions for a shape other than the supplied dimensions.
--
-- >>> :k! Eval (ExceptDims [1,2] [2,3,4])
-- ...
-- = '[0]
data ExceptDims :: [Nat] -> [Nat] -> Exp [Nat]

type instance
  Eval (ExceptDims ds s) =
    Eval (DeleteDims ds =<< EnumFromTo 0 (Eval ((Fcf.-) (Eval (Rank s)) 1)))

-- | Reorder the dimensions of shape according to a list of positions.
--
-- >>> reorder [2,3,4] [2,0,1]
-- [4,2,3]
reorder :: [Int] -> [Int] -> [Int]
reorder [] _ = []
reorder _ [] = []
reorder s (d : ds) = getDim d s : reorder s ds

-- | Reorder the dimensions of shape according to a list of positions.
--
-- >>> :k! Eval (Reorder [2,3,4] [2,0,1])
-- ...
-- = [4, 2, 3]
data Reorder :: [Nat] -> [Nat] -> Exp [Nat]

type instance
  Eval (Reorder ds xs) =
    If
      (Eval (ReorderOk ds xs))
      (Eval (Map (Flip GetDim ds) xs))
      (L.TypeError ('Text "Reorder dimension indices out of bounds"))

-- | Test if a Reorder is valid.
--
-- >>> :k! Eval (ReorderOk [2,3,4] [0,1])
-- ...
-- = False
data ReorderOk :: [Nat] -> [Nat] -> Exp Bool

type instance
  Eval (ReorderOk ds xs) =
    Eval (TyEq (Eval (Rank ds)) (Eval (Rank xs)))
      && Eval (And =<< Map (Flip IsFin (Eval (Rank ds))) xs)

-- | remove 1's from a list
--
-- >>> squeeze [0,1,2,3]
-- [0,2,3]
squeeze :: [Int] -> [Int]
squeeze = filter (/= 1)

-- | Remove 1's from a list.
--
-- >>> :k! (Eval (Squeeze [0,1,2,3]))
-- ...
-- = [0, 2, 3]
data Squeeze :: [a] -> Exp [a]

type instance
  Eval (Squeeze xs) =
    Eval (Filter (Not <=< TyEq 1) xs)

-- | minimum of a list
--
-- >>> S.minimum []
-- *** Exception: zero-ranked
-- ...
-- >>> S.minimum [2,3,4]
-- 2
minimum :: [Int] -> Int
minimum [] = error "zero-ranked"
minimum [x] = x
minimum (x : xs) = P.min x (minimum xs)

-- | minimum of a list
--
-- >>> :k! Eval (Minimum '[])
-- ...
-- = (TypeError ...)
--
-- >>> :k! Eval (Minimum [2,3,4])
-- ...
-- = 2
data Minimum :: [a] -> Exp a

type instance Eval (Minimum '[]) = L.TypeError (L.Text "zero ranked")

type instance
  Eval (Minimum (x ': xs)) =
    Eval (Foldr Min x xs)

-- | Minimum of two type values.
--
-- >>> :k! Eval (Min 0 1)
-- ...
-- = 0
data Min :: a -> a -> Exp a

type instance Eval (Min a b) = If (a <? b) a b

-- | Maximum of two type values.
--
-- >>> :k! Eval (Max 0 1)
-- ...
-- = 1
data Max :: a -> a -> Exp a

type instance Eval (Max a b) = If (a >? b) a b

-- | Check if i is a valid Fin (aka in-bounds index of a dimension)
--
-- >>> isFin 0 2
-- True
-- >>> isFin 2 2
-- False
isFin :: Int -> Int -> Bool
isFin i d = 0 <= i && i + 1 <= d

-- | Check if i is a valid Fin (aka in-bounds index of a dimension)
--
-- >>> :k! Eval (IsFin 0 2)
-- ...
-- = True
-- >>> :k! Eval (IsFin 2 2)
-- ...
-- = False
data IsFin :: Nat -> Nat -> Exp Bool

type instance
  Eval (IsFin x d) =
    x <? d

-- | Check if i is a valid Fins (aka in-bounds index of a Shape)
--
-- >>> isFins [0,1] [2,2]
-- True
-- >>> isFins [0,1] [2,1]
-- False
isFins :: [Int] -> [Int] -> Bool
isFins xs ds = length xs == length ds && and (zipWith isFin xs ds)

-- | Check if i is a valid Fins (aka in-bounds index of a Shape)
--
-- >>> :k! Eval (IsFins [0,1] [2,2])
-- ...
-- = True
-- >>> :k! Eval (IsFins [0,1] [2,1])
-- ...
-- = False
data IsFins :: [Nat] -> [Nat] -> Exp Bool

type instance
  Eval (IsFins xs ds) =
    Eval (And (Eval (ZipWith IsFin xs ds)))
      && Eval (LiftM2 TyEq (Rank xs) (Rank ds))

-- | Is a value a valid dimension of a shape.
--
-- >>> isDim 2 [2,3,4]
-- True
-- >>> isDim 0 []
-- True
isDim :: Int -> [Int] -> Bool
isDim d s = isFin d (rank s) || d == 0 && null s

-- | Is a value a valid dimension of a shape.
--
-- >>> :k! Eval (IsDim 2 [2,3,4])
-- ...
-- = True
-- >>> :k! Eval (IsDim 0 '[])
-- ...
-- = True
data IsDim :: Nat -> [Nat] -> Exp Bool

type instance
  Eval (IsDim d s) =
    Eval (IsFin d =<< Rank s)
      || (0 == d && s == '[])

-- | Are values valid dimensions of a shape.
--
-- >>> isDims [2,1] [2,3,4]
-- True
-- >>> isDims [0] []
-- True
isDims :: [Int] -> [Int] -> Bool
isDims ds s = all (`isDim` s) ds

-- | Are values valid dimensions of a shape.
--
-- >>> :k! Eval (IsDims [2,1] [2,3,4])
-- ...
-- = True
-- >>> :k! Eval (IsDims '[0] '[])
-- ...
-- = True
data IsDims :: [Nat] -> [Nat] -> Exp Bool

type instance
  Eval (IsDims ds s) =
    Eval (And =<< Map (Flip IsDim s) ds)

-- | Get the last position of a dimension of a shape.
--
-- >>> lastPos 2 [2,3,4]
-- 3
-- >>> lastPos 0 []
-- 0
lastPos :: Int -> [Int] -> Int
lastPos d s =
  bool (getDim d s - 1) 0 (0 == d && null s)

-- | Get the last position of a dimension of a shape.
--
-- >>> :k! Eval (LastPos 2 [2,3,4])
-- ...
-- = 3
-- >>> :k! Eval (LastPos 0 '[])
-- ...
-- = 0
data LastPos :: Nat -> [Nat] -> Exp Nat

type instance
  Eval (LastPos d s) =
    If
      (0 == d && s == '[])
      0
      (Eval (GetDim d s) - 1)

-- | Get the minimum dimension as a singleton dimension.
--
-- >>> minDim [2,3,4]
-- [2]
-- >>> minDim []
-- []
minDim :: [Int] -> [Int]
minDim [] = []
minDim s = [minimum s]

-- | Get the minimum dimension as a singleton dimension.
--
-- >>> :k! Eval (MinDim [2,3,4])
-- ...
-- = '[2]
-- >>> :k! Eval (MinDim '[])
-- ...
-- = '[]
data MinDim :: [Nat] -> Exp [Nat]

type instance
  Eval (MinDim s) =
    If
      (s == '[])
      '[]
      '[Eval (Minimum s)]

-- | Enumerate between two Nats
--
-- >>> :k! Eval (EnumFromTo 0 3)
-- ...
-- = [0, 1, 2, 3]
data EnumFromTo :: Nat -> Nat -> Exp [Nat]

type instance Eval (EnumFromTo a b) = Eval (Unfoldr (EnumFromToHelper b) a)

data EnumFromToHelper :: Nat -> Nat -> Exp (Maybe (a, Nat))

type instance
  Eval (EnumFromToHelper b a) =
    If
      (a >? b)
      'Nothing
      ('Just '(a, a + 1))

-- | Left fold.
--
-- >>> :k! Eval (Foldl' (Fcf.+) 0 [1,2,3])
-- ...
-- = 6
data Foldl' :: (b -> a -> Exp b) -> b -> t a -> Exp b

type instance Eval (Foldl' f y '[]) = y

type instance Eval (Foldl' f y (x ': xs)) = Eval (Foldl' f (Eval (f y x)) xs)

-- | Get an element at a given index.
--
-- >>> :kind! Eval (GetIndex 2 [2,3,4])
-- ...
-- = Just 4
data GetIndex :: Nat -> [a] -> Exp (Maybe a)

type instance Eval (GetIndex d xs) = GetIndexImpl d xs

type family GetIndexImpl (n :: Nat) (xs :: [k]) where
  GetIndexImpl _ '[] = 'Nothing
  GetIndexImpl 0 (x ': _) = 'Just x
  GetIndexImpl n (_ ': xs) = GetIndexImpl (n - 1) xs

-- | Get the dimension of a shape at the supplied index. Error if out-of-bounds.
--
-- >>> getDim 1 [2,3,4]
-- 3
-- >>> getDim 3 [2,3,4]
-- *** Exception: getDim outside bounds
-- ...
-- >>> getDim 0 []
-- 1
getDim :: Int -> [Int] -> Int
getDim 0 [] = 1
getDim i s = fromMaybe (error "getDim outside bounds") (maybeGetDim s i)

maybeGetDim :: [a] -> Int -> Maybe a
maybeGetDim xs n
  | n < 0 = Nothing
  | otherwise =
      foldr
        ( \x r k -> case k of
            0 -> Just x
            _ -> r (k - 1)
        )
        (const Nothing)
        xs
        n
{-# INLINEABLE maybeGetDim #-}

-- | Get the dimension of a shape at the supplied index. Error if out-of-bounds or non-computable (usually unknown to the compiler).
--
-- >>> :k! Eval (GetDim 1 [2,3,4])
-- ...
-- = 3
-- >>> :k! Eval (GetDim 3 [2,3,4])
-- ...
-- = (TypeError ...)
-- >>> :k! Eval (GetDim 0 '[])
-- ...
-- = 1
data GetDim :: Nat -> [Nat] -> Exp Nat

type instance
  Eval (GetDim n xs) =
    If
      (Eval (And [Eval (TyEq n 0), Eval (TyEq xs ('[] :: [Nat]))]))
      1
      (Eval (FromMaybe (L.TypeError (L.Text "GetDim out of bounds or non-computable: " :<>: ShowType n :<>: L.Text " " :<>: ShowType xs)) (Eval (GetIndex n xs))))

-- | modify an index at a specific dimension. Errors if out of bounds.
--
-- >>> modifyDim 0 (+1) [0,1,2]
-- [1,1,2]
-- >>> modifyDim 0 (+1) []
-- [2]
modifyDim :: Int -> (Int -> Int) -> [Int] -> [Int]
modifyDim 0 f [] = [f 1]
modifyDim d f xs =
  getDim d xs
    & f
    & (: drop (d + 1) xs)
    & (take d xs <>)

-- | modify an index at a specific dimension. Errors if out of bounds.
--
-- >>> :k! Eval (ModifyDim 0 ((Fcf.+) 1) [0,1,2])
-- ...
-- = [1, 1, 2]
data ModifyDim :: Nat -> (Nat -> Exp Nat) -> [Nat] -> Exp [Nat]

type instance
  Eval (ModifyDim d f s) =
    Eval (LiftM2 (Fcf.++) (Take d s) (LiftM2 Cons (f =<< GetDim d s) (Drop (d + 1) s)))

-- | Increment the index at a dimension of a shape by 1. Scalars turn into singletons.
--
-- >>> incAt 1 [2,3,4]
-- [2,4,4]
-- >>> incAt 0 []
-- [2]
incAt :: Int -> [Int] -> [Int]
incAt d ds = modifyDim d (+ 1) (asSingleton ds)

-- | Increment the index at a dimension of a shape by 1. Scalars turn into singletons.
--
-- >>> :k! Eval (IncAt 1 [2,3,4])
-- ...
-- = [2, 4, 4]
-- >>> :k! Eval (IncAt 0 '[])
-- ...
-- = '[2]
data IncAt :: Nat -> [Nat] -> Exp [Nat]

type instance
  Eval (IncAt d ds) =
    Eval (ModifyDim d ((Fcf.+) 1) (Eval (AsSingleton ds)))

-- | Decrement the index at a dimension os a shape by 1.
--
-- >>> decAt 1 [2,3,4]
-- [2,2,4]
decAt :: Int -> [Int] -> [Int]
decAt d = modifyDim d (\x -> x - 1)

-- | Decrement the index at a dimension of a shape by 1.
--
-- >>> :k! Eval (DecAt 1 [2,3,4])
-- ...
-- = [2, 2, 4]
data DecAt :: Nat -> [Nat] -> Exp [Nat]

type instance
  Eval (DecAt d ds) =
    Eval (ModifyDim d (Flip (Fcf.-) 1) ds)

-- | replace an index at a specific dimension, or transform a scalar into being 1-dimensional.
--
-- >>> setDim 0 1 [2,3,4]
-- [1,3,4]
-- >>> setDim 0 3 []
-- [3]
setDim :: Int -> Int -> [Int] -> [Int]
setDim d x = modifyDim d (const x)

-- | replace an index at a specific dimension.
--
-- >>> :k! Eval (SetDim 0 1 [2,3,4])
-- ...
-- = [1, 3, 4]
data SetDim :: Nat -> Nat -> [Nat] -> Exp [Nat]

type instance
  Eval (SetDim d x ds) =
    Eval (ModifyDim d (ConstFn x) ds)

data SetDimUncurried :: (Nat, Nat) -> [Nat] -> Exp [Nat]

type instance
  Eval (SetDimUncurried xs ds) =
    Eval (SetDim (Eval (Fst xs)) (Eval (Snd xs)) ds)

-- | Take along a dimension.
--
-- >>> takeDim 0 1 [2,3,4]
-- [1,3,4]
takeDim :: Int -> Int -> [Int] -> [Int]
takeDim d t = modifyDim d (min t)

-- | Take along a dimension.
--
-- >>> :k! Eval (TakeDim 0 1 [2,3,4])
-- ...
-- = [1, 3, 4]
data TakeDim :: Nat -> Nat -> [Nat] -> Exp [Nat]

type instance
  Eval (TakeDim d t s) =
    Eval
      (ModifyDim d (Min t) s)

-- | Drop along a dimension.
--
-- >>> dropDim 2 1 [2,3,4]
-- [2,3,3]
dropDim :: Int -> Int -> [Int] -> [Int]
dropDim d t = modifyDim d (max 0 . (\x -> x - t))

-- | Drop along a dimension.
--
-- >>> :k! Eval (DropDim 2 1 [2,3,4])
-- ...
-- = [2, 3, 3]
data DropDim :: Nat -> Nat -> [Nat] -> Exp [Nat]

type instance
  Eval (DropDim d t s) =
    Eval
      ( ModifyDim
          d
          (Max 0 <=< Flip (Fcf.-) t)
          s
      )

-- | delete the i'th dimension. No effect on a scalar.
--
-- >>> deleteDim 1 [2, 3, 4]
-- [2,4]
-- >>> deleteDim 2 []
-- []
deleteDim :: Int -> [Int] -> [Int]
deleteDim i s = take i s ++ drop (i + 1) s

-- | delete the i'th dimension
--
-- >>> :k! Eval (DeleteDim 1 [2, 3, 4])
-- ...
-- = [2, 4]
-- >>> :k! Eval (DeleteDim 1 '[])
-- ...
-- = '[]
data DeleteDim :: Nat -> [Nat] -> Exp [Nat]

type instance
  Eval (DeleteDim i ds) =
    Eval (LiftM2 (Fcf.++) (Take i ds) (Drop (i + 1) ds))

-- | Insert a new dimension at a position (or at the end if > rank).
--
-- >>> insertDim 1 3 [2,4]
-- [2,3,4]
-- >>> insertDim 0 4 []
-- [4]
insertDim :: Int -> Int -> [Int] -> [Int]
insertDim d i s = take d s ++ (i : drop d s)

-- | Insert a new dimension at a position (or at the end if > rank).
--
-- >>> :k! Eval (InsertDim 1 3 [2,4])
-- ...
-- = [2, 3, 4]
-- >>> :k! Eval (InsertDim 0 4 '[])
-- ...
-- = '[4]
data InsertDim :: Nat -> Nat -> [Nat] -> Exp [Nat]

type instance
  Eval (InsertDim d i ds) =
    Eval (LiftM2 (Fcf.++) (Take d ds) (Cons i =<< Drop d ds))

data InsertDimUncurried :: (Nat, Nat) -> [Nat] -> Exp [Nat]

type instance
  Eval (InsertDimUncurried xs ds) =
    Eval (InsertDim (Eval (Fst xs)) (Eval (Snd xs)) ds)

-- | Is a slice ok constraint.
--
-- >>> :k! Eval (InsertOk 2 [2,3,4] [2,3])
-- ...
-- = True
-- >>> :k! Eval (InsertOk 0 '[] '[])
-- ...
-- = True
data InsertOk :: Nat -> [Nat] -> [Nat] -> Exp Bool

type instance
  Eval (InsertOk d s si) =
    Eval
      ( And
          [ Eval (IsDim d s),
            Eval (TyEq si (Eval (DeleteDim d s)))
          ]
      )

-- | Is a slice ok?
--
-- >>> :k! Eval (SliceOk 1 1 2 [2,3,4])
-- ...
-- = True
data SliceOk :: Nat -> Nat -> Nat -> [Nat] -> Exp Bool

type instance
  Eval (SliceOk d off l s) =
    Eval
      ( And
          [ Eval (IsFin off =<< GetDim d s),
            Eval ((Fcf.<) l =<< GetDim d s),
            Eval ((Fcf.<) (off + l) (Eval (GetDim d s) + 1)),
            Eval (IsDim d s)
          ]
      )

-- | Combine elements of two lists pairwise.
data ZipWith3 :: (a -> b -> c -> Exp d) -> [a] -> [b] -> [c] -> Exp [d]

type instance Eval (ZipWith3 _f '[] _bs _cs) = '[]

type instance Eval (ZipWith3 _f _as '[] _cs) = '[]

type instance Eval (ZipWith3 _f _as _bs '[]) = '[]

type instance
  Eval (ZipWith3 f (a ': as) (b ': bs) (c ': cs)) =
    Eval (f a b c) ': Eval (ZipWith3 f as bs cs)

data SliceOk_ :: [Nat] -> Nat -> Nat -> Nat -> Exp Bool

type instance Eval (SliceOk_ s d off l) = Eval (SliceOk d off l s)

-- | Are slices ok?
--
-- >>> :k! Eval (SlicesOk '[1] '[1] '[2] [2,3,4])
-- ...
-- = True
data SlicesOk :: [Nat] -> [Nat] -> [Nat] -> [Nat] -> Exp Bool

type instance
  Eval (SlicesOk ds offs ls s) =
    Eval (And =<< ZipWith3 (SliceOk_ s) ds offs ls)

-- | concatenate two arrays at dimension i
--
-- Bespoke logic for scalars.
--
-- >>> concatenate 1 [2,3,4] [2,3,4]
-- [2,6,4]
-- >>> concatenate 0 [3] []
-- [4]
-- >>> concatenate 0 [] [3]
-- [4]
-- >>> concatenate 0 [] []
-- [2]
concatenate :: Int -> [Int] -> [Int] -> [Int]
concatenate _ [] [] = [2]
concatenate _ [] [x] = [x + 1]
concatenate _ [x] [] = [x + 1]
concatenate i s0 s1 = take i s0 ++ (getDim i s0 + getDim i s1 : drop (i + 1) s0)

-- | concatenate two arrays at dimension i
--
-- Bespoke logic for scalars.
--
-- >>> :k! Eval (Concatenate 1 [2,3,4] [2,3,4])
-- ...
-- = [2, 6, 4]
-- >>> :k! Eval (Concatenate 0 '[3] '[])
-- ...
-- = '[4]
-- >>> :k! Eval (Concatenate 0 '[] '[3])
-- ...
-- = '[4]
-- >>> :k! Eval (Concatenate 0 '[] '[])
-- ...
-- = '[2]
data Concatenate :: Nat -> [Nat] -> [Nat] -> Exp [Nat]

type instance
  Eval (Concatenate i s0 s1) =
    If
      (Eval (ConcatenateOk i s0 s1))
      (Eval (Eval (Take i s0) ++ (Eval (GetDim i s0) + Eval (GetDim i s1) : Eval (Drop (i + 1) s0))))
      (L.TypeError (L.Text "Concatenate Mis-matched shapes."))

-- | Concatenate is Ok if ranks are the same and the non-indexed portion of the shapes are the same.
data ConcatenateOk :: Nat -> [Nat] -> [Nat] -> Exp Bool

type instance
  Eval (ConcatenateOk i s0 s1) =
    Eval (IsDim i s0)
      && Eval (IsDim i s1)
      && Eval (LiftM2 TyEq (DeleteDim i s0) (DeleteDim i s1))
      && Eval (LiftM2 TyEq (Rank =<< AsSingleton s0) (Rank =<< AsSingleton s1))

-- * multiple dimension manipulations

-- | Get dimensions of a shape.
--
-- >>> getDims [2,0] [2,3,4]
-- [4,2]
-- >>> getDims [2] []
-- []
getDims :: [Int] -> [Int] -> [Int]
getDims _ [] = []
getDims i s = (`getDim` s) <$> i

-- | Get dimensions of a shape.
--
-- >>> :k! Eval (GetDims [2,0] [2,3,4])
-- ...
-- = [4, 2]
-- >>> :k! Eval (GetDims '[2] '[])
-- ...
-- = '[(TypeError ...)]
data GetDims :: [Nat] -> [Nat] -> Exp [Nat]

type instance
  Eval (GetDims xs ds) =
    Eval (Map (Flip GetDim ds) xs)

-- | Get the index of the last position in the selected dimensions of a shape. Errors on a 0-dimension.
--
-- >>> getLastPositions [2,0] [2,3,4]
-- [3,1]
-- >>> getLastPositions [0] [0]
-- [-1]
getLastPositions :: [Int] -> [Int] -> [Int]
getLastPositions ds s =
  fmap (\x -> x - 1) (getDims ds s)

-- | Get the index of the last position in the selected dimensions of a shape. Errors on a 0-dimension.
--
-- >>> :k! Eval (GetLastPositions [2,0] [2,3,4])
-- ...
-- = [3, 1]
data GetLastPositions :: [Nat] -> [Nat] -> Exp [Nat]

type instance
  Eval (GetLastPositions ds s) =
    Eval (Map (Flip (Fcf.-) 1) (Eval (GetDims ds s)))

-- | modify dimensions of a shape with (separate) functions.
--
-- >>> modifyDims [0,1] [(+1), (+5)] [2,3,4]
-- [3,8,4]
modifyDims :: [Int] -> [Int -> Int] -> [Int] -> [Int]
modifyDims ds fs ns = foldl' (\ns' (d, f) -> modifyDim d f ns') ns (zip ds fs)

-- | Convert a list of positions that reference deletions according to a final shape to 1 that references deletions relative to an initial shape.
--
-- To delete the positions [1,2,5] from a list, for example, you need to delete position 1, (arriving at a 4 element list), then position 1, arriving at a 3 element list, and finally position 3.
--
-- >>> preDeletePositions [1,2,5]
-- [1,1,3]
--
-- >>> preDeletePositions [1,2,0]
-- [1,1,0]
preDeletePositions :: [Int] -> [Int]
preDeletePositions as = reverse (go as [])
  where
    go [] r = r
    go (x : xs) r = go (decPast x <$> xs) (x : r)
    decPast x y = bool (y - 1) y (y < x)

-- | Convert a list of positions that reference deletions according to a final shape to 1 that references deletions relative to an initial shape.
--
-- To delete the positions [1,2,5] from a list, for example, you need to delete position 1, (arriving at a 4 element list), then position 1, arriving at a 3 element list, and finally position 3.
--
-- >>> :k! Eval (PreDeletePositions [1,2,5])
-- ...
-- = [1, 1, 3]
--
-- >>> :k! Eval (PreDeletePositions [1,2,0])
-- ...
-- = [1, 1, 0]
data PreDeletePositions :: [Nat] -> Exp [Nat]

type instance
  Eval (PreDeletePositions xs) =
    Eval (Reverse (Eval (PreDeletePositionsGo xs '[])))

data PreDeletePositionsGo :: [Nat] -> [Nat] -> Exp [Nat]

type instance Eval (PreDeletePositionsGo '[] rs) = rs

type instance
  Eval (PreDeletePositionsGo (x : xs) r) =
    Eval (PreDeletePositionsGo (Eval (Map (DecPast x) xs)) (x : r))

data DecPast :: Nat -> Nat -> Exp Nat

type instance
  Eval (DecPast x d) =
    If (x + 1 <=? d) (d - 1) d

-- | Convert a list of position that reference insertions according to a final shape to 1 that references list insertions relative to an initial shape.
--
-- To insert into positions [1,2,0] from a list, starting from a 2 element list, for example, you need to insert at position 0, (arriving at a 3 element list), then position 1, arriving at a 4 element list, and finally position 0.
--
-- > preInsertPositions == reverse . preDeletePositions . reverse
-- >>> preInsertPositions [1,2,5]
-- [1,2,5]
--
-- >>> preInsertPositions [1,2,0]
-- [0,1,0]
preInsertPositions :: [Int] -> [Int]
preInsertPositions = reverse . preDeletePositions . reverse

-- | Convert a list of position that reference insertions according to a final shape to 1 that references list insertions relative to an initial shape.
--
-- To insert into positions [1,2,0] from a list, starting from a 2 element list, for example, you need to insert at position 0, (arriving at a 3 element list), then position 1, arriving at a 4 element list, and finally position 0.
--
-- > preInsertPositions == reverse . preDeletePositions . reverse
-- >>> :k! Eval (PreInsertPositions [1,2,5])
-- ...
-- = [1, 2, 5]
--
-- >>> :k! Eval (PreInsertPositions [1,2,0])
-- ...
-- = [0, 1, 0]
data PreInsertPositions :: [Nat] -> Exp [Nat]

type instance
  Eval (PreInsertPositions xs) =
    Eval (Reverse =<< (PreDeletePositions =<< Reverse xs))

-- | drop dimensions of a shape according to a list of positions (where position refers to the initial shape)
--
-- >>> deleteDims [1,0] [2, 3, 4]
-- [4]
deleteDims :: [Int] -> [Int] -> [Int]
deleteDims i s = foldl' (flip deleteDim) s (preDeletePositions i)

-- | drop dimensions of a shape according to a list of positions (where position refers to the initial shape)
--
-- >>> :k! Eval (DeleteDims [1,0] [2, 3, 4])
-- ...
-- = '[4]
data DeleteDims :: [Nat] -> [Nat] -> Exp [Nat]

type instance
  Eval (DeleteDims xs ds) =
    Eval (Foldl' (Flip DeleteDim) ds =<< PreDeletePositions xs)

-- | Insert a list of dimensions according to dimensions and positions.  Note that the list of positions references the final shape and not the initial shape.
--
-- >>> insertDims [0] [5] []
-- [5]
-- >>> insertDims [1,0] [3,2] [4]
-- [2,3,4]
insertDims :: [Int] -> [Int] -> [Int] -> [Int]
insertDims ds xs s = foldl' (flip (uncurry insertDim)) s ps
  where
    ps = zip (preInsertPositions ds) xs

-- | insert a list of dimensions according to dimension,position tuple lists.  Note that the list of positions references the final shape and not the initial shape.
--
-- >>> :k! Eval (InsertDims '[0] '[5] '[])
-- ...
-- = '[5]
-- >>> :k! Eval (InsertDims [1,0] [3,2] '[4])
-- ...
-- = [2, 3, 4]
data InsertDims :: [Nat] -> [Nat] -> [Nat] -> Exp [Nat]

type instance
  Eval (InsertDims ds xs s) =
    Eval (Foldl' (Flip InsertDimUncurried) s =<< Flip Zip xs =<< PreInsertPositions ds)

-- | Set dimensions of a shape.
--
-- >>> setDims [0,1] [1,5] [2,3,4]
-- [1,5,4]
--
-- >>> setDims [0] [3] []
-- [3]
setDims :: [Int] -> [Int] -> [Int] -> [Int]
setDims ds xs ns = foldl' (\ns' (d, x) -> setDim d x ns') ns (zip ds xs)

-- | Set dimensions of a shape.
--
-- >>> :k! Eval (SetDims [0,1] [1,5] [2,3,4])
-- ...
-- = [1, 5, 4]
--
-- >>> :k! Eval (SetDims '[0] '[3] '[])
-- ...
-- = '[3]
data SetDims :: [Nat] -> [Nat] -> [Nat] -> Exp [Nat]

type instance
  Eval (SetDims ds xs ns) =
    Eval (Foldl' (Flip SetDimUncurried) ns =<< Zip ds xs)

-- | Drop a number of elements of a shape along the supplied dimensions.
--
-- >>> dropDims [0,2] [1,3] [2,3,4]
-- [1,3,1]
dropDims :: [Int] -> [Int] -> [Int] -> [Int]
dropDims ds xs s = setDims ds xs' s
  where
    xs' = zipWith (-) (getDims ds s) xs

-- | Drop a number of elements of a shape along the supplied dimensions.
--
-- >>> :k! Eval (DropDims [0,2] [1,3] [2,3,4])
-- ...
-- = [1, 3, 1]
data DropDims :: [Nat] -> [Nat] -> [Nat] -> Exp [Nat]

type instance
  Eval (DropDims ds xs s) =
    Eval (SetDims ds (Eval (ZipWith (Fcf.-) (Eval (GetDims ds s)) xs)) s)

-- | Concatenate and replace dimensions, creating a new dimension at the supplied postion.
--
-- >>> concatDims [0,1] 1 [2,3,4]
-- [4,6]
concatDims :: [Int] -> Int -> [Int] -> [Int]
concatDims ds n s = insertDim n (size $ getDims ds s) (deleteDims ds s)

-- | Drop a number of elements of a shape along the supplied dimensions.
--
-- >>> :k! Eval (ConcatDims [0,1] 1 [2,3,4])
-- ...
-- = [4, 6]
data ConcatDims :: [Nat] -> Nat -> [Nat] -> Exp [Nat]

type instance
  Eval (ConcatDims ds n s) =
    Eval (InsertDim n (Eval (Size (Eval (GetDims ds s)))) (Eval (DeleteDims ds s)))

-- | Unconcatenate and reinsert dimensions for an index.
--
-- >>> unconcatDimsIndex [0,1] 1 [4,6] [2,3]
-- [0,3,2]
unconcatDimsIndex :: [Int] -> Int -> [Int] -> [Int] -> [Int]
unconcatDimsIndex ds n s i = insertDims ds (shapen (getDims ds s) (getDim n i)) (deleteDim n i)

-- | reverse an index along specific dimensions.
--
-- >>> reverseIndex [0] [2,3,4] [0,1,2]
-- [1,1,2]
reverseIndex :: [Int] -> [Int] -> [Int] -> [Int]
reverseIndex ds ns xs = fmap (\(i, x, n) -> bool x (n - 1 - x) (i `elem` ds)) (zip3 [0 ..] xs ns)

-- | rotate a list
--
-- >>> rotate 1 [0..3]
-- [1,2,3,0]
-- >>> rotate (-1) [0..3]
-- [3,0,1,2]
rotate :: Int -> [a] -> [a]
rotate r xs = drop r' xs <> take r' xs
  where
    r' = r `mod` List.length xs

-- | rotate an index along a specific dimension.
--
-- >>> rotateIndex 0 1 [2,3,4] [0,1,2]
-- [1,1,2]
rotateIndex :: Int -> Int -> [Int] -> [Int] -> [Int]
rotateIndex d r s = modifyDim d (\x -> (x + r) `mod` getDim d s)

-- | rotate an index along specific dimensions.
--
-- >>> rotatesIndex [0] [1] [2,3,4] [0,1,2]
-- [1,1,2]
rotatesIndex :: [Int] -> [Int] -> [Int] -> [Int] -> [Int]
rotatesIndex ds rs s xs = foldr (\(d, r) acc -> rotateIndex d r s acc) xs (zip ds rs)

-- | Test whether an index is a diagonal one.
--
-- >>> isDiag [2,2,2]
-- True
-- >>> isDiag [1,2]
-- False
isDiag :: (Eq a) => [a] -> Bool
isDiag [] = True
isDiag [_] = True
isDiag [x, y] = x == y
isDiag (x : y : xs) = x == y && isDiag (y : xs)

-- | Expanded shape of a windowed array
--
-- >>> expandWindows [2,2] [4,3,2]
-- [3,2,2,2,2]
expandWindows :: [Int] -> [Int] -> [Int]
expandWindows ws ds = List.zipWith (\s' x' -> s' - x' + 1) ds ws <> ws <> List.drop (rank ws) ds

-- | Expanded shape of a windowed array
--
-- >>> :k! Eval (ExpandWindows [2,2] [4,3,2])
-- ...
-- = [3, 2, 2, 2, 2]
data ExpandWindows :: [Nat] -> [Nat] -> Exp [Nat]

type instance
  Eval (ExpandWindows ws ds) =
    Eval (Eval (ZipWith (Fcf.-) (Eval (Map ((Fcf.+) 1) ds)) ws) ++ Eval (ws ++ Eval (Drop (Eval (Rank ws)) ds)))

-- | Index into windows of an expanded windowed array, given a rank of the windows.
--
-- >>> indexWindows 2 [0,1,2,1,1]
-- [2,2,1]
indexWindows :: Int -> [Int] -> [Int]
indexWindows r ds = List.zipWith (+) (List.take r ds) (List.take r (List.drop r ds)) <> List.drop (r + r) ds

-- | Dimensions of a windowed array.
--
-- >>> dimWindows [2,2] [2,3,4]
-- [0,1,2]
dimWindows :: [Int] -> [Int] -> [Int]
dimWindows ws s = range (rank s) <> [rank s * 2 .. (rank ws - 1)]

-- | Dimensions of a windowed array.
--
-- >>> :k! Eval (DimWindows [2,2] [4,3,2])
-- ...
-- = [0, 1, 2]
data DimWindows :: [Nat] -> [Nat] -> Exp [Nat]

type instance
  Eval (DimWindows ws s) =
    Eval (Eval (Range =<< Rank s) ++ Eval (EnumFromTo (Eval ((Fcf.*) 2 (Eval (Rank s)))) (Eval (Rank ws) - 1)))

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Data
  ( module Data.Typeable,
    Data (..),
    Constr,
    DataType,
    Fixity (..),
    constrFields,
    constrFixity,
    constrIndex,
    dataTypeConstrs,
    dataTypeName,
    indexConstr,
    mkConstr,
    mkDataType,
    mkNoRepType,
    showConstr,
  )
where

import Control.Monad (MonadPlus (..))
import Data.Either (Either (..))
import Data.Maybe (Maybe (..))
import Data.Typeable
import GHC.Base (Monad (..), String, const, id, otherwise)
import GHC.Err (errorWithoutStackTrace)
import GHC.Float ()
import GHC.ForeignPtr (ForeignPtr)
import GHC.Int (Int, Int16, Int32, Int64, Int8)
import GHC.Internal.Classes (Eq (..))
import GHC.Internal.Data.NonEmpty (NonEmpty (..))
import GHC.Num ((+))
import GHC.Num.Integer (Integer)
import GHC.Num.Natural (Natural)
import GHC.Prim.Real (Ratio (..))
import GHC.Ptr (Ptr)
import GHC.Real (Integral, (%))
import GHC.Show (Show (..))
import GHC.Types (Bool (..), Char, Double, Float, Ordering (..))
import GHC.Word (Word, Word16, Word32, Word64, Word8)

-- | Generic operations on a data type.
class (Typeable a) => Data a where
  gfoldl ::
    (forall d b. (Data d) => c (d -> b) -> d -> c b) ->
    (forall g. g -> c g) ->
    a ->
    c a
  gfoldl _ z = z
  gunfold ::
    (forall b r. (Data b) => c (b -> r) -> c r) ->
    (forall r. r -> c r) ->
    Constr ->
    c a
  toConstr :: a -> Constr
  dataTypeOf :: a -> DataType

  -- An instance for a unary or binary type constructor gives these casts
  -- with 'gcast1' or 'gcast2'.
  dataCast1 :: (Typeable t) => (forall d. (Data d) => c (t d)) -> Maybe (c a)
  dataCast1 _ = Nothing
  dataCast2 :: (Typeable t) => (forall d e. (Data d, Data e) => c (t d e)) -> Maybe (c a)
  dataCast2 _ = Nothing

  -- The generic maps below are GHC's defaults, each written with 'gfoldl'.
  gmapT :: (forall b. (Data b) => b -> b) -> a -> a
  gmapT f x0 = unID (gfoldl k ID x0)
    where
      k :: (Data d) => ID (d -> b) -> d -> ID b
      k (ID c) x = ID (c (f x))

  gmapQl :: forall r r'. (r -> r' -> r) -> r -> (forall d. (Data d) => d -> r') -> a -> r
  gmapQl o acc f x0 = unCONST (gfoldl k (\_ -> CONST acc) x0)
    where
      k :: (Data d) => CONST r (d -> b) -> d -> CONST r b
      k c x = CONST (unCONST c `o` f x)

  gmapQr :: forall r r'. (r' -> r -> r) -> r -> (forall d. (Data d) => d -> r') -> a -> r
  gmapQr o acc f x0 = unQr (gfoldl k (const (Qr id)) x0) acc
    where
      k :: (Data d) => Qr r (d -> b) -> d -> Qr r b
      k (Qr c) x = Qr (\acc' -> c (f x `o` acc'))

  gmapQ :: (forall d. (Data d) => d -> u) -> a -> [u]
  gmapQ f = gmapQr (:) [] f

  gmapQi :: forall u. Int -> (forall d. (Data d) => d -> u) -> a -> u
  gmapQi i f x = case gfoldl k (\_ -> Qi 0 Nothing) x of
    Qi _ (Just q) -> q
    Qi _ Nothing -> errorWithoutStackTrace "Data.Data.gmapQi: index out of range"
    where
      k :: (Data d) => Qi u (d -> b) -> d -> Qi u b
      k (Qi i' q) y = Qi (i' + 1) (if i == i' then Just (f y) else q)

  gmapM :: forall m. (Monad m) => (forall d. (Data d) => d -> m d) -> a -> m a
  gmapM f = gfoldl k return
    where
      k :: (Data d) => m (d -> b) -> d -> m b
      k c x = c >>= \c' -> f x >>= \x' -> return (c' x')

  gmapMp :: forall m. (MonadPlus m) => (forall d. (Data d) => d -> m d) -> a -> m a
  gmapMp f x = unMp (gfoldl k (\g -> Mp (return (g, False))) x) >>= \(x', b) -> if b then return x' else mzero
    where
      k :: (Data d) => Mp m (d -> b) -> d -> Mp m b
      k (Mp c) y =
        Mp
          ( c >>= \(h, b) ->
              (f y >>= \y' -> return (h y', True)) `mplus` return (h y, b)
          )

  gmapMo :: forall m. (MonadPlus m) => (forall d. (Data d) => d -> m d) -> a -> m a
  gmapMo f x = unMp (gfoldl k (\g -> Mp (return (g, False))) x) >>= \(x', b) -> if b then return x' else mzero
    where
      k :: (Data d) => Mp m (d -> b) -> d -> Mp m b
      k (Mp c) y =
        Mp
          ( c >>= \(h, b) ->
              if b
                then return (h y, b)
                else (f y >>= \y' -> return (h y', True)) `mplus` return (h y, b)
          )

-- The helper types that thread 'gfoldl' through the generic maps.
newtype ID x = ID {unID :: x}

newtype CONST c a = CONST {unCONST :: c}

data Qi q a = Qi Int (Maybe q)

newtype Qr r a = Qr {unQr :: r -> r}

newtype Mp m x = Mp {unMp :: m (x, Bool)}

-- | The fixity of a data constructor.
data Fixity = Prefix | Infix

-- | The description of a data type.
data DataType = DataType String [Constr]

-- | The description of one data constructor.
data Constr = Constr String [String] Fixity Int

-- | Make a data type that lists its constructors.
mkDataType :: String -> [Constr] -> DataType
mkDataType = DataType

-- | Make a data type that has no generic representation.
mkNoRepType :: String -> DataType
mkNoRepType name = DataType name []

-- | Make a constructor description.
-- As in GHC, the index is the position of the constructor name in the list
-- of the data type. Thus a constructor can name the data type that lists
-- it. If the list does not have the name, the standin gives the index that
-- comes after the listed constructors. A no-rep data type lists no
-- constructors, thus its first constructor gets index 1.
mkConstr :: DataType -> String -> [String] -> Fixity -> Constr
mkConstr dataType name fields fixity =
  Constr name fields fixity (constrPosition 1 (dataTypeConstrs dataType))
  where
    constrPosition index [] = index
    constrPosition index (constr : rest)
      | showConstr constr == name = index
      | otherwise = constrPosition (index + 1) rest

-- | Give the constructor that has an index in a data type.
indexConstr :: DataType -> Int -> Constr
indexConstr dataType index = select 1 (dataTypeConstrs dataType)
  where
    select _ [] = errorWithoutStackTrace "Data.Data.indexConstr: index out of range"
    select position (constr : rest)
      | position == index = constr
      | otherwise = select (position + 1) rest

-- | Give the name of a data type.
dataTypeName :: DataType -> String
dataTypeName (DataType name _) = name

-- | Give the constructors that a data type lists.
dataTypeConstrs :: DataType -> [Constr]
dataTypeConstrs (DataType _ constrs) = constrs

-- | Give the name of a constructor.
showConstr :: Constr -> String
showConstr (Constr name _ _ _) = name

-- | Give the field names of a constructor.
constrFields :: Constr -> [String]
constrFields (Constr _ fields _ _) = fields

-- | Give the fixity of a constructor.
constrFixity :: Constr -> Fixity
constrFixity (Constr _ _ fixity _) = fixity

-- | Give the index of a constructor in its data type.
constrIndex :: Constr -> Int
constrIndex (Constr _ _ _ index) = index

instance (Data a) => Data [a] where
  gfoldl _ z [] = z []
  gfoldl f z (x : xs) = z (:) `f` x `f` xs
  toConstr [] = nilConstr
  toConstr (_ : _) = consConstr
  gunfold k z c = case constrIndex c of
    1 -> z []
    2 -> k (k (z (:)))
    _ -> errorWithoutStackTrace "Data.Data.gunfold(List)"
  dataTypeOf _ = listDataType
  dataCast1 f = gcast1 f

nilConstr :: Constr
nilConstr = mkConstr listDataType "[]" [] Prefix

consConstr :: Constr
consConstr = mkConstr (DataType "Prelude.[]" [nilConstr]) "(:)" [] Infix

listDataType :: DataType
listDataType = mkDataType "Prelude.[]" [nilConstr, consConstr]

instance Data Bool where
  toConstr False = falseConstr
  toConstr True = trueConstr
  gunfold _ z c = case constrIndex c of
    1 -> z False
    2 -> z True
    _ -> errorWithoutStackTrace "Data.Data.gunfold(Bool)"
  dataTypeOf _ = boolDataType

falseConstr :: Constr
falseConstr = mkConstr boolDataType "False" [] Prefix

trueConstr :: Constr
trueConstr = mkConstr (DataType "Prelude.Bool" [falseConstr]) "True" [] Prefix

boolDataType :: DataType
boolDataType = mkDataType "Prelude.Bool" [falseConstr, trueConstr]

-- The primitive types show their value as the constructor name. The
-- standin cannot rebuild a value from a constructor, so gunfold fails.
instance Data Char where
  toConstr x = mkConstr charType ['\'', x, '\''] [] Prefix
  gunfold _ _ _ = errorWithoutStackTrace "Data.Data.gunfold(Char)"
  dataTypeOf _ = charType

charType :: DataType
charType = mkNoRepType "Prelude.Char"

instance Data Int where
  toConstr x = mkConstr intType (show x) [] Prefix
  gunfold _ _ _ = errorWithoutStackTrace "Data.Data.gunfold(Int)"
  dataTypeOf _ = intType

intType :: DataType
intType = mkNoRepType "Prelude.Int"

instance Data Word where
  toConstr x = mkConstr wordType (show x) [] Prefix
  gunfold _ _ _ = errorWithoutStackTrace "Data.Data.gunfold(Word)"
  dataTypeOf _ = wordType

wordType :: DataType
wordType = mkNoRepType "Prelude.Word"

instance Data Word8 where
  toConstr x = mkConstr word8Type (show x) [] Prefix
  gunfold _ _ _ = errorWithoutStackTrace "Data.Data.gunfold(Word8)"
  dataTypeOf _ = word8Type

word8Type :: DataType
word8Type = mkNoRepType "Data.Word.Word8"

instance (Data a, Data b) => Data (a, b) where
  gfoldl f z (a, b) = z (,) `f` a `f` b
  gunfold k z c = case constrIndex c of
    1 -> k (k (z (,)))
    _ -> errorWithoutStackTrace "Data.Data.gunfold((,))"
  toConstr _ = pairConstr
  dataTypeOf _ = pairDataType
  dataCast2 f = gcast2 f

pairConstr :: Constr
pairConstr = mkConstr (mkNoRepType "Prelude.(,)") "(,)" [] Infix

pairDataType :: DataType
pairDataType = mkDataType "Prelude.(,)" [pairConstr]

-- The standin describes the other primitive number types in the same way.
instance Data Int8 where
  toConstr x = mkConstr int8Type (show x) [] Prefix
  gunfold _ _ _ = errorWithoutStackTrace "Data.Data.gunfold(Int8)"
  dataTypeOf _ = int8Type

int8Type :: DataType
int8Type = mkNoRepType "Data.Int.Int8"

instance Data Int16 where
  toConstr x = mkConstr int16Type (show x) [] Prefix
  gunfold _ _ _ = errorWithoutStackTrace "Data.Data.gunfold(Int16)"
  dataTypeOf _ = int16Type

int16Type :: DataType
int16Type = mkNoRepType "Data.Int.Int16"

instance Data Int32 where
  toConstr x = mkConstr int32Type (show x) [] Prefix
  gunfold _ _ _ = errorWithoutStackTrace "Data.Data.gunfold(Int32)"
  dataTypeOf _ = int32Type

int32Type :: DataType
int32Type = mkNoRepType "Data.Int.Int32"

instance Data Int64 where
  toConstr x = mkConstr int64Type (show x) [] Prefix
  gunfold _ _ _ = errorWithoutStackTrace "Data.Data.gunfold(Int64)"
  dataTypeOf _ = int64Type

int64Type :: DataType
int64Type = mkNoRepType "Data.Int.Int64"

instance Data Word16 where
  toConstr x = mkConstr word16Type (show x) [] Prefix
  gunfold _ _ _ = errorWithoutStackTrace "Data.Data.gunfold(Word16)"
  dataTypeOf _ = word16Type

word16Type :: DataType
word16Type = mkNoRepType "Data.Word.Word16"

instance Data Word32 where
  toConstr x = mkConstr word32Type (show x) [] Prefix
  gunfold _ _ _ = errorWithoutStackTrace "Data.Data.gunfold(Word32)"
  dataTypeOf _ = word32Type

word32Type :: DataType
word32Type = mkNoRepType "Data.Word.Word32"

instance Data Word64 where
  toConstr x = mkConstr word64Type (show x) [] Prefix
  gunfold _ _ _ = errorWithoutStackTrace "Data.Data.gunfold(Word64)"
  dataTypeOf _ = word64Type

word64Type :: DataType
word64Type = mkNoRepType "Data.Word.Word64"

instance Data Integer where
  toConstr x = mkConstr integerType (show x) [] Prefix
  gunfold _ _ _ = errorWithoutStackTrace "Data.Data.gunfold(Integer)"
  dataTypeOf _ = integerType

integerType :: DataType
integerType = mkNoRepType "Prelude.Integer"

instance Data Natural where
  toConstr x = mkConstr naturalType (show x) [] Prefix
  gunfold _ _ _ = errorWithoutStackTrace "Data.Data.gunfold(Natural)"
  dataTypeOf _ = naturalType

naturalType :: DataType
naturalType = mkNoRepType "Numeric.Natural.Natural"

instance Data Float where
  toConstr x = mkConstr floatType (show x) [] Prefix
  gunfold _ _ _ = errorWithoutStackTrace "Data.Data.gunfold(Float)"
  dataTypeOf _ = floatType

floatType :: DataType
floatType = mkNoRepType "Prelude.Float"

instance Data Double where
  toConstr x = mkConstr doubleType (show x) [] Prefix
  gunfold _ _ _ = errorWithoutStackTrace "Data.Data.gunfold(Double)"
  dataTypeOf _ = doubleType

doubleType :: DataType
doubleType = mkNoRepType "Prelude.Double"

-- GHC derives the instances of these algebraic types. Stock deriving
-- cannot write them here, because the generated code names the
-- descriptions of this module, and they are not available before the
-- module is checked. Thus the instances have the shape that GHC derives.
instance Data () where
  toConstr () = unitConstr
  gunfold _ z _ = z ()
  dataTypeOf _ = unitDataType

unitConstr :: Constr
unitConstr = mkConstr unitDataType "()" [] Prefix

unitDataType :: DataType
unitDataType = mkDataType "Prelude.()" [unitConstr]

instance Data Ordering where
  toConstr LT = ltConstr
  toConstr EQ = eqConstr
  toConstr GT = gtConstr
  gunfold _ z c = case constrIndex c of
    1 -> z LT
    2 -> z EQ
    _ -> z GT
  dataTypeOf _ = orderingDataType

ltConstr :: Constr
ltConstr = mkConstr orderingDataType "LT" [] Prefix

eqConstr :: Constr
eqConstr = mkConstr orderingDataType "EQ" [] Prefix

gtConstr :: Constr
gtConstr = mkConstr orderingDataType "GT" [] Prefix

orderingDataType :: DataType
orderingDataType = mkDataType "Prelude.Ordering" [ltConstr, eqConstr, gtConstr]

instance (Data a) => Data (Maybe a) where
  gfoldl _ z Nothing = z Nothing
  gfoldl k z (Just x) = z Just `k` x
  toConstr Nothing = nothingConstr
  toConstr (Just _) = justConstr
  gunfold k z c = case constrIndex c of
    1 -> z Nothing
    _ -> k (z Just)
  dataTypeOf _ = maybeDataType
  dataCast1 f = gcast1 f

nothingConstr :: Constr
nothingConstr = mkConstr maybeDataType "Nothing" [] Prefix

justConstr :: Constr
justConstr = mkConstr maybeDataType "Just" [] Prefix

maybeDataType :: DataType
maybeDataType = mkDataType "Prelude.Maybe" [nothingConstr, justConstr]

instance (Data a, Data b) => Data (Either a b) where
  gfoldl k z (Left x) = z Left `k` x
  gfoldl k z (Right x) = z Right `k` x
  toConstr (Left _) = leftConstr
  toConstr (Right _) = rightConstr
  gunfold k z c = case constrIndex c of
    1 -> k (z Left)
    _ -> k (z Right)
  dataTypeOf _ = eitherDataType
  dataCast2 f = gcast2 f

leftConstr :: Constr
leftConstr = mkConstr eitherDataType "Left" [] Prefix

rightConstr :: Constr
rightConstr = mkConstr eitherDataType "Right" [] Prefix

eitherDataType :: DataType
eitherDataType = mkDataType "Prelude.Either" [leftConstr, rightConstr]

instance (Data a) => Data (NonEmpty a) where
  gfoldl k z (x :| xs) = z (:|) `k` x `k` xs
  toConstr _ = nonEmptyConstr
  gunfold k z _ = k (k (z (:|)))
  dataTypeOf _ = nonEmptyDataType
  dataCast1 f = gcast1 f

nonEmptyConstr :: Constr
nonEmptyConstr = mkConstr nonEmptyDataType ":|" [] Infix

nonEmptyDataType :: DataType
nonEmptyDataType = mkDataType "GHC.Internal.Base.NonEmpty" [nonEmptyConstr]

instance (Data a, Data b, Data c) => Data (a, b, c) where
  gfoldl k z (a, b, c) = z (,,) `k` a `k` b `k` c
  toConstr _ = tripleConstr
  gunfold k z _ = k (k (k (z (,,))))
  dataTypeOf _ = tripleDataType

tripleConstr :: Constr
tripleConstr = mkConstr tripleDataType "(,,)" [] Infix

tripleDataType :: DataType
tripleDataType = mkDataType "Prelude.(,,)" [tripleConstr]

-- As in GHC, the instance rebuilds a ratio with '(%)', which reduces it.
instance (Data a, Integral a) => Data (Ratio a) where
  gfoldl k z (Ratio numerator denominator) = z (%) `k` numerator `k` denominator
  toConstr _ = ratioConstr
  gunfold k z _ = k (k (z (%)))
  dataTypeOf _ = ratioDataType

ratioConstr :: Constr
ratioConstr = mkConstr ratioDataType ":%" [] Infix

ratioDataType :: DataType
ratioDataType = mkDataType "GHC.Real.Ratio" [ratioConstr]

-- As in GHC, pointers are abstract. They have no constructor to show or
-- rebuild.
instance (Data a) => Data (Ptr a) where
  toConstr _ = errorWithoutStackTrace "Data.Data.toConstr(Ptr)"
  gunfold _ _ _ = errorWithoutStackTrace "Data.Data.gunfold(Ptr)"
  dataTypeOf _ = mkNoRepType "GHC.Ptr.Ptr"
  dataCast1 f = gcast1 f

instance (Data a) => Data (ForeignPtr a) where
  toConstr _ = errorWithoutStackTrace "Data.Data.toConstr(ForeignPtr)"
  gunfold _ _ _ = errorWithoutStackTrace "Data.Data.gunfold(ForeignPtr)"
  dataTypeOf _ = mkNoRepType "GHC.ForeignPtr.ForeignPtr"
  dataCast1 f = gcast1 f

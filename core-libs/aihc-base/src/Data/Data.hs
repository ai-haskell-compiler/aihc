{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Data
  ( module Data.Typeable,
    Data (..),
    DataType,
    mkDataType,
    mkIntType,
    mkFloatType,
    mkCharType,
    mkNoRepType,
    dataTypeName,
    DataRep (..),
    dataTypeRep,
    repConstr,
    isAlgType,
    dataTypeConstrs,
    indexConstr,
    maxConstrIndex,
    isNorepType,
    Constr,
    ConIndex,
    Fixity (..),
    mkConstr,
    mkConstrTag,
    mkIntegralConstr,
    mkRealConstr,
    mkCharConstr,
    constrType,
    ConstrRep (..),
    constrRep,
    constrFields,
    constrFixity,
    constrIndex,
    showConstr,
    readConstr,
    tyconUQname,
    tyconModule,
    fromConstr,
    fromConstrB,
    fromConstrM,
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
import GHC.Internal.Classes (Eq (..), Ord (..))
import GHC.Internal.Data.NonEmpty (NonEmpty (..))
import GHC.Num (Num (..))
import GHC.Num.Integer (Integer)
import GHC.Num.Natural (Natural)
import GHC.Prim.Real (Ratio (..))
import GHC.Ptr (Ptr)
import GHC.Read (Read)
import GHC.Real (Integral (..), Rational, Real (..), fromIntegral, (%))
import GHC.Show (Show (..))
import GHC.Types (Bool (..), Char, Double, Float, Ordering (..))
import GHC.Word (Word, Word16, Word32, Word64, Word8)
import Prelude (Fractional (..), break, dropWhile, length, reads, showParen, showString, shows, tail, (++), (.))

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

instance Eq Fixity where
  Prefix == Prefix = True
  Infix == Infix = True
  _ == _ = False

instance Show Fixity where
  showsPrec _ Prefix = showString "Prefix"
  showsPrec _ Infix = showString "Infix"

-- | The description of a data type: its name and its representation.
data DataType = DataType String DataRep

instance Show DataType where
  showsPrec d (DataType name representation) =
    showParen (d >= 11) (showString "DataType {tycon = " . shows name . showString ", datarep = " . shows representation . showString "}")

-- | The representation of a data type.
data DataRep
  = AlgRep [Constr]
  | IntRep
  | FloatRep
  | CharRep
  | NoRep

instance Eq DataRep where
  AlgRep left == AlgRep right = left == right
  IntRep == IntRep = True
  FloatRep == FloatRep = True
  CharRep == CharRep = True
  NoRep == NoRep = True
  _ == _ = False

instance Show DataRep where
  showsPrec d (AlgRep constrs) = showParen (d >= 11) (showString "AlgRep " . showsPrec 11 constrs)
  showsPrec _ IntRep = showString "IntRep"
  showsPrec _ FloatRep = showString "FloatRep"
  showsPrec _ CharRep = showString "CharRep"
  showsPrec _ NoRep = showString "NoRep"

-- | The index of an algebraic constructor. The first constructor has index 1.
type ConIndex = Int

-- | The representation of a constructor.
data ConstrRep
  = AlgConstr ConIndex
  | IntConstr Integer
  | FloatConstr Rational
  | CharConstr Char

instance Eq ConstrRep where
  AlgConstr left == AlgConstr right = left == right
  IntConstr left == IntConstr right = left == right
  FloatConstr left == FloatConstr right = left == right
  CharConstr left == CharConstr right = left == right
  _ == _ = False

instance Show ConstrRep where
  showsPrec d (AlgConstr index) = showParen (d >= 11) (showString "AlgConstr " . showsPrec 11 index)
  showsPrec d (IntConstr value) = showParen (d >= 11) (showString "IntConstr " . showsPrec 11 value)
  showsPrec d (FloatConstr value) = showParen (d >= 11) (showString "FloatConstr " . showsPrec 11 value)
  showsPrec d (CharConstr value) = showParen (d >= 11) (showString "CharConstr " . showsPrec 11 value)

-- | The description of one data constructor: its representation, its name,
-- its field names, its fixity, and its data type.
data Constr = Constr ConstrRep String [String] Fixity DataType

-- | As in GHC, two constructors are equal when their representations are
-- equal.
instance Eq Constr where
  left == right = constrRep left == constrRep right

instance Show Constr where
  showsPrec _ constr = showString (showConstr constr)

-- | Make a data type that lists its constructors.
mkDataType :: String -> [Constr] -> DataType
mkDataType name constrs = DataType name (AlgRep constrs)

-- | Make the data type of a primitive integral type.
mkIntType :: String -> DataType
mkIntType name = DataType name IntRep

-- | Make the data type of a primitive floating-point type.
mkFloatType :: String -> DataType
mkFloatType name = DataType name FloatRep

-- | Make the data type of a character type.
mkCharType :: String -> DataType
mkCharType name = DataType name CharRep

-- | Make a data type that has no generic representation.
mkNoRepType :: String -> DataType
mkNoRepType name = DataType name NoRep

-- | Give the name of a data type.
dataTypeName :: DataType -> String
dataTypeName (DataType name _) = name

-- | Give the representation of a data type.
dataTypeRep :: DataType -> DataRep
dataTypeRep (DataType _ representation) = representation

-- | Tell if a data type is algebraic.
isAlgType :: DataType -> Bool
isAlgType dataType = case dataTypeRep dataType of
  AlgRep _ -> True
  _ -> False

-- | Tell if a data type has no generic representation.
isNorepType :: DataType -> Bool
isNorepType dataType = case dataTypeRep dataType of
  NoRep -> True
  _ -> False

-- | Give the constructors of an algebraic data type.
dataTypeConstrs :: DataType -> [Constr]
dataTypeConstrs dataType = case dataTypeRep dataType of
  AlgRep constrs -> constrs
  _ -> errorWithoutStackTrace ("Data.Data.dataTypeConstrs is not supported for " ++ dataTypeName dataType ++ ", as it is not an algebraic data type.")

-- | Give the constructor that has an index in an algebraic data type.
indexConstr :: DataType -> ConIndex -> Constr
indexConstr dataType index = select 1 (dataTypeConstrs dataType)
  where
    select _ [] = errorWithoutStackTrace "Data.Data.indexConstr: index out of range"
    select position (constr : rest)
      | position == index = constr
      | otherwise = select (position + 1) rest

-- | Give the largest constructor index of an algebraic data type.
maxConstrIndex :: DataType -> ConIndex
maxConstrIndex dataType = length (dataTypeConstrs dataType)

-- | Make a constructor description.
-- As in GHC, the index is the position of the constructor name in the list
-- of the data type. Thus a constructor can name the data type that lists
-- it. If the list does not have the name, the standin gives the index that
-- comes after the listed constructors. A data type that is not algebraic
-- lists no constructors, thus its first constructor gets index 1.
mkConstr :: DataType -> String -> [String] -> Fixity -> Constr
mkConstr dataType name fields fixity =
  Constr (AlgConstr (constrPosition 1 listed)) name fields fixity dataType
  where
    listed = case dataTypeRep dataType of
      AlgRep constrs -> constrs
      _ -> []
    constrPosition index [] = index
    constrPosition index (constr : rest)
      | showConstr constr == name = index
      | otherwise = constrPosition (index + 1) rest

-- | Make a constructor description with a known index.
mkConstrTag :: DataType -> String -> ConIndex -> [String] -> Fixity -> Constr
mkConstrTag dataType name index fields fixity =
  Constr (AlgConstr index) name fields fixity dataType

-- | Make the constructor of a value of a primitive type. The constructor has
-- no fields and has the prefix fixity.
mkPrimCon :: DataType -> String -> ConstrRep -> Constr
mkPrimCon dataType name representation = Constr representation name [] Prefix dataType

-- | Make the constructor of an integral value.
mkIntegralConstr :: (Integral a, Show a) => DataType -> a -> Constr
mkIntegralConstr dataType value = case dataTypeRep dataType of
  IntRep -> mkPrimCon dataType (show value) (IntConstr (toInteger value))
  _ -> errorWithoutStackTrace ("Data.Data.mkIntegralConstr is not supported for " ++ dataTypeName dataType ++ ", as it is not an Integral data type.")

-- | Make the constructor of a real value.
mkRealConstr :: (Real a, Show a) => DataType -> a -> Constr
mkRealConstr dataType value = case dataTypeRep dataType of
  FloatRep -> mkPrimCon dataType (show value) (FloatConstr (toRational value))
  _ -> errorWithoutStackTrace ("Data.Data.mkRealConstr is not supported for " ++ dataTypeName dataType ++ ", as it is not a Real data type.")

-- | Make the constructor of a character.
mkCharConstr :: DataType -> Char -> Constr
mkCharConstr dataType value = case dataTypeRep dataType of
  CharRep -> mkPrimCon dataType (show value) (CharConstr value)
  _ -> errorWithoutStackTrace ("Data.Data.mkCharConstr is not supported for " ++ dataTypeName dataType ++ ", as it is not a Char data type.")

-- | Give the constructor of a data type that has a representation.
repConstr :: DataType -> ConstrRep -> Constr
repConstr dataType representation = case (dataTypeRep dataType, representation) of
  (AlgRep _, AlgConstr index) -> indexConstr dataType index
  (IntRep, IntConstr value) -> mkIntegralConstr dataType value
  (FloatRep, FloatConstr value) -> mkRealConstr dataType value
  (CharRep, CharConstr value) -> mkCharConstr dataType value
  _ -> errorWithoutStackTrace "Data.Data.repConstr: The given ConstrRep does not fit to the given DataType."

-- | Give the constructor that has a name in a data type. For a primitive
-- data type, read the value from the name.
readConstr :: DataType -> String -> Maybe Constr
readConstr dataType name = case dataTypeRep dataType of
  AlgRep constrs -> find constrs
  IntRep -> readValue (\value -> mkPrimCon dataType name (IntConstr (value :: Integer)))
  FloatRep -> readValue (\value -> mkPrimCon dataType name (FloatConstr (toRational (value :: Double))))
  CharRep -> readValue (mkPrimCon dataType name . CharConstr)
  NoRep -> Nothing
  where
    find [] = Nothing
    find (constr : rest)
      | showConstr constr == name = Just constr
      | otherwise = find rest
    readValue :: (Read t) => (t -> Constr) -> Maybe Constr
    readValue make = case reads name of
      [(value, "")] -> Just (make value)
      _ -> Nothing

-- | Give the data type of a constructor.
constrType :: Constr -> DataType
constrType (Constr _ _ _ _ dataType) = dataType

-- | Give the representation of a constructor.
constrRep :: Constr -> ConstrRep
constrRep (Constr representation _ _ _ _) = representation

-- | Give the name of a constructor.
showConstr :: Constr -> String
showConstr (Constr _ name _ _ _) = name

-- | Give the field names of a constructor.
constrFields :: Constr -> [String]
constrFields (Constr _ _ fields _ _) = fields

-- | Give the fixity of a constructor.
constrFixity :: Constr -> Fixity
constrFixity (Constr _ _ _ fixity _) = fixity

-- | Give the index of an algebraic constructor in its data type.
constrIndex :: Constr -> ConIndex
constrIndex constr = case constrRep constr of
  AlgConstr index -> index
  _ -> errorWithoutStackTrace ("Data.Data.constrIndex is not supported for " ++ dataTypeName (constrType constr) ++ ", as it is not an algebraic data type.")

-- | Remove the module qualifier from a type constructor name.
tyconUQname :: String -> String
tyconUQname name = case dropWhile (/= '.') name of
  [] -> name
  rest -> tyconUQname (tail rest)

-- | Give the module qualifier of a type constructor name.
tyconModule :: String -> String
tyconModule name = case break (== '.') name of
  (_, []) -> []
  (prefix, rest) -> prefix ++ qualifier (tail rest)
  where
    qualifier remaining = case tyconModule remaining of
      [] -> []
      found -> '.' : found

-- | Build a value from a constructor without fields.
fromConstr :: (Data a) => Constr -> a
fromConstr = fromConstrB (errorWithoutStackTrace "Data.Data.fromConstr")

-- | Build a value from a constructor. Give the same value to each field.
fromConstrB :: (Data a) => (forall d. (Data d) => d) -> Constr -> a
fromConstrB field constr = unID (gunfold k ID constr)
  where
    k :: (Data b) => ID (b -> r) -> ID r
    k (ID function) = ID (function field)

-- | Build a value from a constructor. Run the same action for each field.
fromConstrM :: forall m a. (Monad m, Data a) => (forall d. (Data d) => m d) -> Constr -> m a
fromConstrM field = gunfold k return
  where
    k :: (Data b) => m (b -> r) -> m r
    k function = function >>= \function' -> field >>= \value -> return (function' value)

-- The constructor of a primitive value gives the value back.
integralFromConstr :: (Num a) => String -> Constr -> a
integralFromConstr typeName constr = case constrRep constr of
  IntConstr value -> fromInteger value
  _ -> errorWithoutStackTrace ("Data.Data.gunfold: Constructor " ++ showConstr constr ++ " is not of type " ++ typeName ++ ".")

fractionalFromConstr :: (Fractional a) => String -> Constr -> a
fractionalFromConstr typeName constr = case constrRep constr of
  FloatConstr value -> fromRational value
  _ -> errorWithoutStackTrace ("Data.Data.gunfold: Constructor " ++ showConstr constr ++ " is not of type " ++ typeName ++ ".")

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
consConstr = mkConstr listDataType "(:)" [] Infix

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
trueConstr = mkConstr boolDataType "True" [] Prefix

boolDataType :: DataType
boolDataType = mkDataType "Prelude.Bool" [falseConstr, trueConstr]

-- As in GHC, the constructor of a primitive value has the value as its
-- representation and shows the value as its name.
instance Data Char where
  toConstr = mkCharConstr charType
  gunfold _ z c = case constrRep c of
    CharConstr x -> z x
    _ -> errorWithoutStackTrace ("Data.Data.gunfold: Constructor " ++ showConstr c ++ " is not of type Char.")
  dataTypeOf _ = charType

charType :: DataType
charType = mkCharType "Prelude.Char"

instance Data Int where
  toConstr = mkIntegralConstr intType
  gunfold _ z c = z (integralFromConstr "Int" c)
  dataTypeOf _ = intType

intType :: DataType
intType = mkIntType "Prelude.Int"

instance Data Word where
  toConstr = mkIntegralConstr wordType
  gunfold _ z c = z (integralFromConstr "Word" c)
  dataTypeOf _ = wordType

wordType :: DataType
wordType = mkIntType "Prelude.Word"

instance Data Word8 where
  toConstr = mkIntegralConstr word8Type
  gunfold _ z c = z (integralFromConstr "Word8" c)
  dataTypeOf _ = word8Type

word8Type :: DataType
word8Type = mkIntType "Data.Word.Word8"

instance (Data a, Data b) => Data (a, b) where
  gfoldl f z (a, b) = z (,) `f` a `f` b
  gunfold k z c = case constrIndex c of
    1 -> k (k (z (,)))
    _ -> errorWithoutStackTrace "Data.Data.gunfold((,))"
  toConstr _ = pairConstr
  dataTypeOf _ = pairDataType
  dataCast2 f = gcast2 f

pairConstr :: Constr
pairConstr = mkConstr pairDataType "(,)" [] Infix

pairDataType :: DataType
pairDataType = mkDataType "Prelude.(,)" [pairConstr]

instance Data Int8 where
  toConstr = mkIntegralConstr int8Type
  gunfold _ z c = z (integralFromConstr "Int8" c)
  dataTypeOf _ = int8Type

int8Type :: DataType
int8Type = mkIntType "Data.Int.Int8"

instance Data Int16 where
  toConstr = mkIntegralConstr int16Type
  gunfold _ z c = z (integralFromConstr "Int16" c)
  dataTypeOf _ = int16Type

int16Type :: DataType
int16Type = mkIntType "Data.Int.Int16"

instance Data Int32 where
  toConstr = mkIntegralConstr int32Type
  gunfold _ z c = z (integralFromConstr "Int32" c)
  dataTypeOf _ = int32Type

int32Type :: DataType
int32Type = mkIntType "Data.Int.Int32"

instance Data Int64 where
  toConstr = mkIntegralConstr int64Type
  gunfold _ z c = z (integralFromConstr "Int64" c)
  dataTypeOf _ = int64Type

int64Type :: DataType
int64Type = mkIntType "Data.Int.Int64"

instance Data Word16 where
  toConstr = mkIntegralConstr word16Type
  gunfold _ z c = z (integralFromConstr "Word16" c)
  dataTypeOf _ = word16Type

word16Type :: DataType
word16Type = mkIntType "Data.Word.Word16"

instance Data Word32 where
  toConstr = mkIntegralConstr word32Type
  gunfold _ z c = z (integralFromConstr "Word32" c)
  dataTypeOf _ = word32Type

word32Type :: DataType
word32Type = mkIntType "Data.Word.Word32"

instance Data Word64 where
  toConstr = mkIntegralConstr word64Type
  gunfold _ z c = z (integralFromConstr "Word64" c)
  dataTypeOf _ = word64Type

word64Type :: DataType
word64Type = mkIntType "Data.Word.Word64"

instance Data Integer where
  toConstr = mkIntegralConstr integerType
  gunfold _ z c = z (integralFromConstr "Integer" c)
  dataTypeOf _ = integerType

integerType :: DataType
integerType = mkIntType "Prelude.Integer"

instance Data Natural where
  toConstr = mkIntegralConstr naturalType
  gunfold _ z c = z (integralFromConstr "Natural" c)
  dataTypeOf _ = naturalType

naturalType :: DataType
naturalType = mkIntType "Numeric.Natural.Natural"

instance Data Float where
  toConstr = mkRealConstr floatType
  gunfold _ z c = z (fractionalFromConstr "Float" c)
  dataTypeOf _ = floatType

floatType :: DataType
floatType = mkFloatType "Prelude.Float"

instance Data Double where
  toConstr = mkRealConstr doubleType
  gunfold _ z c = z (fractionalFromConstr "Double" c)
  dataTypeOf _ = doubleType

doubleType :: DataType
doubleType = mkFloatType "Prelude.Double"

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

instance (Data a, Data b, Data c, Data d) => Data (a, b, c, d) where
  gfoldl k z (a, b, c, d) = z (,,,) `k` a `k` b `k` c `k` d
  toConstr _ = tuple4Constr
  gunfold k z _ = k (k (k (k (z (,,,)))))
  dataTypeOf _ = tuple4DataType

tuple4Constr :: Constr
tuple4Constr = mkConstr tuple4DataType "(,,,)" [] Infix

tuple4DataType :: DataType
tuple4DataType = mkDataType "Prelude.(,,,)" [tuple4Constr]

instance (Data a, Data b, Data c, Data d, Data e) => Data (a, b, c, d, e) where
  gfoldl k z (a, b, c, d, e) = z (,,,,) `k` a `k` b `k` c `k` d `k` e
  toConstr _ = tuple5Constr
  gunfold k z _ = k (k (k (k (k (z (,,,,))))))
  dataTypeOf _ = tuple5DataType

tuple5Constr :: Constr
tuple5Constr = mkConstr tuple5DataType "(,,,,)" [] Infix

tuple5DataType :: DataType
tuple5DataType = mkDataType "Prelude.(,,,,)" [tuple5Constr]

instance (Data a, Data b, Data c, Data d, Data e, Data f) => Data (a, b, c, d, e, f) where
  gfoldl k z (a, b, c, d, e, f) = z (,,,,,) `k` a `k` b `k` c `k` d `k` e `k` f
  toConstr _ = tuple6Constr
  gunfold k z _ = k (k (k (k (k (k (z (,,,,,)))))))
  dataTypeOf _ = tuple6DataType

tuple6Constr :: Constr
tuple6Constr = mkConstr tuple6DataType "(,,,,,)" [] Infix

tuple6DataType :: DataType
tuple6DataType = mkDataType "Prelude.(,,,,,)" [tuple6Constr]

instance (Data a, Data b, Data c, Data d, Data e, Data f, Data g) => Data (a, b, c, d, e, f, g) where
  gfoldl k z (a, b, c, d, e, f, g) = z (,,,,,,) `k` a `k` b `k` c `k` d `k` e `k` f `k` g
  toConstr _ = tuple7Constr
  gunfold k z _ = k (k (k (k (k (k (k (z (,,,,,,))))))))
  dataTypeOf _ = tuple7DataType

tuple7Constr :: Constr
tuple7Constr = mkConstr tuple7DataType "(,,,,,,)" [] Infix

tuple7DataType :: DataType
tuple7DataType = mkDataType "Prelude.(,,,,,,)" [tuple7Constr]

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

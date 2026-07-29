{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Prelude
  ( Applicative (..),
    Bool (..),
    Char (..),
    Either (..),
    Eq (..),
    Functor (..),
    IO,
    Int,
    Integral (..),
    Integer,
    List (..),
    Maybe (..),
    Monad (..),
    Num (..),
    Ord (..),
    Ordering (..),
    Show (..),
    ShowS,
    String,
    (&&),
    (.),
    (++),
    (=<<),
    (/=),
    (==),
    id,
    fromIntegral,
    not,
    otherwise,
    showChar,
    showParen,
    shows,
    showString,
    (||),
  )
where

import Data.Bool (Bool (..), not, otherwise, (&&), (||))
import Data.Kind (Type)
import GHC.IO (IO (..))
import GHC.Int (Int (..))
import GHC.Integer (Integer)
import GHC.Internal.Integer (Integer (..), compareInteger#, eqInteger#, integerAbs, integerQuotRemWord#)
import GHC.Num (Num (..))
import GHC.Prim (RealWorld, State#, chr#, compareInt#, int2Word#, ord#, word2Int#, (+#), (<#), (==#))
import GHC.Real (Integral (..), fromIntegral)
import GHC.Tuple ()

data Char = C# Char#

data List a = [] | a : [a]

infixr 5 :

type String = [Char]

id :: a -> a
id x = x

(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = compose
  where
    compose value = f (g value)

infixr 9 .

data Maybe a = Nothing | Just a

data Either a b = Left a | Right b

data Ordering = LT | EQ | GT

class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool

infix 4 ==, /=

instance Eq Bool where
  False == False = True
  False == True = False
  True == False = False
  True == True = True

  x /= y = not (x == y)

instance Eq Int where
  I# x == I# y =
    case (==#) x y of
      0# -> False
      _ -> True

  x /= y = not (x == y)

instance Eq Integer where
  x == y =
    case eqInteger# x y of
      0# -> False
      _ -> True

  x /= y = not (x == y)

instance Eq Char where
  C# x == C# y =
    case (==#) (ord# x) (ord# y) of
      0# -> False
      _ -> True

  x /= y = not (x == y)

instance (Eq a) => Eq [a] where
  [] == [] = True
  [] == (_ : _) = False
  (_ : _) == [] = False
  (x : xs) == (y : ys) = x == y && xs == ys

  xs /= ys = not (xs == ys)

instance (Eq a) => Eq (Maybe a) where
  Nothing == Nothing = True
  Nothing == Just _ = False
  Just _ == Nothing = False
  Just x == Just y = x == y

  x /= y = not (x == y)

instance (Eq a, Eq b) => Eq (Either a b) where
  Left x == Left y = x == y
  Left _ == Right _ = False
  Right _ == Left _ = False
  Right x == Right y = x == y

  x /= y = not (x == y)

instance Eq Ordering where
  LT == LT = True
  EQ == EQ = True
  GT == GT = True
  _ == _ = False

  x /= y = not (x == y)

class (Eq a) => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a

infix 4 <, <=, >, >=

instance Ord Bool where
  compare = compareBool
  x < y = lessBy compareBool x y
  x <= y = lessOrEqualBy compareBool x y
  x > y = greaterBy compareBool x y
  x >= y = greaterOrEqualBy compareBool x y
  max = maxBy compareBool
  min = minBy compareBool

instance Ord Int where
  compare = compareInt
  x < y = lessBy compareInt x y
  x <= y = lessOrEqualBy compareInt x y
  x > y = greaterBy compareInt x y
  x >= y = greaterOrEqualBy compareInt x y
  max = maxBy compareInt
  min = minBy compareInt

instance Ord Integer where
  compare = compareInteger
  x < y = lessBy compareInteger x y
  x <= y = lessOrEqualBy compareInteger x y
  x > y = greaterBy compareInteger x y
  x >= y = greaterOrEqualBy compareInteger x y
  max = maxBy compareInteger
  min = minBy compareInteger

instance (Ord a) => Ord [a] where
  compare = compareList
  xs < ys = lessBy compareList xs ys
  xs <= ys = lessOrEqualBy compareList xs ys
  xs > ys = greaterBy compareList xs ys
  xs >= ys = greaterOrEqualBy compareList xs ys
  max = maxBy compareList
  min = minBy compareList

instance (Ord a) => Ord (Maybe a) where
  compare = compareMaybe
  x < y = lessBy compareMaybe x y
  x <= y = lessOrEqualBy compareMaybe x y
  x > y = greaterBy compareMaybe x y
  x >= y = greaterOrEqualBy compareMaybe x y
  max = maxBy compareMaybe
  min = minBy compareMaybe

instance (Ord a, Ord b) => Ord (Either a b) where
  compare = compareEither
  x < y = lessBy compareEither x y
  x <= y = lessOrEqualBy compareEither x y
  x > y = greaterBy compareEither x y
  x >= y = greaterOrEqualBy compareEither x y
  max = maxBy compareEither
  min = minBy compareEither

instance Ord Ordering where
  compare = compareOrdering
  x < y = lessBy compareOrdering x y
  x <= y = lessOrEqualBy compareOrdering x y
  x > y = greaterBy compareOrdering x y
  x >= y = greaterOrEqualBy compareOrdering x y
  max = maxBy compareOrdering
  min = minBy compareOrdering

compareBool :: Bool -> Bool -> Ordering
compareBool False False = EQ
compareBool False True = LT
compareBool True False = GT
compareBool True True = EQ

compareInt :: Int -> Int -> Ordering
compareInt (I# x) (I# y) = orderingFromInt# (compareInt# x y)

compareInteger :: Integer -> Integer -> Ordering
compareInteger x y = orderingFromInt# (compareInteger# x y)

compareList :: (Ord a) => [a] -> [a] -> Ordering
compareList [] [] = EQ
compareList [] (_ : _) = LT
compareList (_ : _) [] = GT
compareList (x : xs) (y : ys) =
  case compare x y of
    LT -> LT
    EQ -> compareList xs ys
    GT -> GT

compareMaybe :: (Ord a) => Maybe a -> Maybe a -> Ordering
compareMaybe Nothing Nothing = EQ
compareMaybe Nothing (Just _) = LT
compareMaybe (Just _) Nothing = GT
compareMaybe (Just x) (Just y) = compare x y

compareEither :: (Ord a, Ord b) => Either a b -> Either a b -> Ordering
compareEither (Left x) (Left y) = compare x y
compareEither (Left _) (Right _) = LT
compareEither (Right _) (Left _) = GT
compareEither (Right x) (Right y) = compare x y

compareOrdering :: Ordering -> Ordering -> Ordering
compareOrdering LT LT = EQ
compareOrdering LT _ = LT
compareOrdering EQ LT = GT
compareOrdering EQ EQ = EQ
compareOrdering EQ GT = LT
compareOrdering GT GT = EQ
compareOrdering GT _ = GT

orderingFromInt# :: Int# -> Ordering
orderingFromInt# value =
  case value of
    0# -> EQ
    1# -> GT
    _ -> LT

lessBy :: (a -> a -> Ordering) -> a -> a -> Bool
lessBy cmp x y =
  case cmp x y of
    LT -> True
    _ -> False

lessOrEqualBy :: (a -> a -> Ordering) -> a -> a -> Bool
lessOrEqualBy cmp x y =
  case cmp x y of
    GT -> False
    _ -> True

greaterBy :: (a -> a -> Ordering) -> a -> a -> Bool
greaterBy cmp x y =
  case cmp x y of
    GT -> True
    _ -> False

greaterOrEqualBy :: (a -> a -> Ordering) -> a -> a -> Bool
greaterOrEqualBy cmp x y =
  case cmp x y of
    LT -> False
    _ -> True

maxBy :: (a -> a -> Ordering) -> a -> a -> a
maxBy cmp x y =
  case cmp x y of
    GT -> x
    _ -> y

minBy :: (a -> a -> Ordering) -> a -> a -> a
minBy cmp x y =
  case cmp x y of
    GT -> y
    _ -> x

type ShowS = String -> String

class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS

  showsPrec _ value suffix = show value ++ suffix
  show value = showsPrec (I# 0#) value []
  showList = showListWith shows

shows :: (Show a) => a -> ShowS
shows = showsPrec (I# 0#)

showChar :: Char -> ShowS
showChar char suffix = char : suffix

showString :: String -> ShowS
showString = (++)

showParen :: Bool -> ShowS -> ShowS
showParen condition output =
  case condition of
    False -> output
    True -> showChar '(' . output . showChar ')'

instance Show Bool where
  showsPrec _ False = showString "False"
  showsPrec _ True = showString "True"

instance Show Int where
  showsPrec precedence (I# value) = showsSignedInteger precedence (IS value)

instance Show Integer where
  showsPrec = showsSignedInteger

instance Show () where
  showsPrec _ () = showString "()"

instance Show Ordering where
  showsPrec _ LT = showString "LT"
  showsPrec _ EQ = showString "EQ"
  showsPrec _ GT = showString "GT"

instance Show Char where
  showsPrec _ char = showChar '\'' . showLitChar char . showChar '\''
  showList chars = showChar '"' . showLitString chars . showChar '"'

instance (Show a) => Show [a] where
  showsPrec _ = showList

instance (Show a) => Show (Maybe a) where
  showsPrec _ Nothing = showString "Nothing"
  showsPrec precedence (Just value) =
    showParen (precedence > 10) (showString "Just " . showsPrec 11 value)

instance (Show a, Show b) => Show (Either a b) where
  showsPrec precedence (Left value) =
    showParen (precedence > 10) (showString "Left " . showsPrec 11 value)
  showsPrec precedence (Right value) =
    showParen (precedence > 10) (showString "Right " . showsPrec 11 value)

instance (Show a, Show b) => Show (a, b) where
  showsPrec _ (first, second) =
    showChar '(' . shows first . showChar ',' . shows second . showChar ')'

instance (Show a, Show b, Show c) => Show (a, b, c) where
  showsPrec _ (first, second, third) =
    showChar '('
      . shows first
      . showChar ','
      . shows second
      . showChar ','
      . shows third
      . showChar ')'

showsSignedInteger :: Int -> Integer -> ShowS
showsSignedInteger precedence value =
  case (<#) (compareInteger# value (IS 0#)) 0# of
    0# -> showsUnsignedInteger value
    _ -> showParen (precedence > 6) (showChar '-' . showsUnsignedInteger (integerAbs value))

showsUnsignedInteger :: Integer -> ShowS
showsUnsignedInteger value suffix =
  case integerQuotRemWord# value (int2Word# 10#) of
    (# quotient, remainder #) ->
      case eqInteger# quotient (IS 0#) of
        1# -> digitChar remainder : suffix
        _ -> showsUnsignedInteger quotient (digitChar remainder : suffix)

digitChar :: Word# -> Char
digitChar digit = C# (chr# ((+#) (word2Int# digit) 48#))

showListWith :: (a -> ShowS) -> [a] -> ShowS
showListWith _ [] = showString "[]"
showListWith showElement (value : values) =
  showChar '[' . showElement value . showListTail showElement values

showListTail :: (a -> ShowS) -> [a] -> ShowS
showListTail _ [] = showChar ']'
showListTail showElement (value : values) =
  showChar ',' . showElement value . showListTail showElement values

showLitString :: String -> ShowS
showLitString [] = id
showLitString ('"' : chars) = showString "\\\"" . showLitString chars
showLitString ('\'' : chars) = showChar '\'' . showLitString chars
showLitString (char : chars) = showLitChar char . showLitString chars

showLitChar :: Char -> ShowS
showLitChar '\a' = showString "\\a"
showLitChar '\b' = showString "\\b"
showLitChar '\f' = showString "\\f"
showLitChar '\n' = showString "\\n"
showLitChar '\r' = showString "\\r"
showLitChar '\t' = showString "\\t"
showLitChar '\v' = showString "\\v"
showLitChar '\\' = showString "\\\\"
showLitChar '\'' = showString "\\'"
showLitChar char@(C# value) =
  case ord# value of
    code -> showLitCode char code

showLitCode :: Char -> Int# -> ShowS
showLitCode char code =
  case (<#) code 32# of
    1# -> showChar '\\' . showString (asciiControlName code)
    _ ->
      case (==#) code 127# of
        1# -> showString "\\DEL"
        _ ->
          case (<#) code 160# of
            1# -> showNumericEscape code
            _ -> showChar char

asciiControlName :: Int# -> String
asciiControlName code =
  case code of
    0# -> "NUL"
    1# -> "SOH"
    2# -> "STX"
    3# -> "ETX"
    4# -> "EOT"
    5# -> "ENQ"
    6# -> "ACK"
    7# -> "BEL"
    8# -> "BS"
    9# -> "HT"
    10# -> "LF"
    11# -> "VT"
    12# -> "FF"
    13# -> "CR"
    14# -> "SO"
    15# -> "SI"
    16# -> "DLE"
    17# -> "DC1"
    18# -> "DC2"
    19# -> "DC3"
    20# -> "DC4"
    21# -> "NAK"
    22# -> "SYN"
    23# -> "ETB"
    24# -> "CAN"
    25# -> "EM"
    26# -> "SUB"
    27# -> "ESC"
    28# -> "FS"
    29# -> "GS"
    30# -> "RS"
    _ -> "US"

showNumericEscape :: Int# -> ShowS
showNumericEscape value suffix =
  showChar '\\' (showsUnsignedInteger (IS value) (protectNumericEscape suffix))

protectNumericEscape :: String -> String
protectNumericEscape [] = []
protectNumericEscape chars@('0' : _) = '\\' : '&' : chars
protectNumericEscape chars@('1' : _) = '\\' : '&' : chars
protectNumericEscape chars@('2' : _) = '\\' : '&' : chars
protectNumericEscape chars@('3' : _) = '\\' : '&' : chars
protectNumericEscape chars@('4' : _) = '\\' : '&' : chars
protectNumericEscape chars@('5' : _) = '\\' : '&' : chars
protectNumericEscape chars@('6' : _) = '\\' : '&' : chars
protectNumericEscape chars@('7' : _) = '\\' : '&' : chars
protectNumericEscape chars@('8' : _) = '\\' : '&' : chars
protectNumericEscape chars@('9' : _) = '\\' : '&' : chars
protectNumericEscape chars = chars

(++) :: [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x : xs) ys = x : (xs ++ ys)

class Functor (f :: Type -> Type) where
  fmap :: (a -> b) -> f a -> f b

instance Functor List where
  fmap = fmapList

instance Functor Maybe where
  fmap f mx =
    case mx of
      Nothing -> Nothing
      Just x -> Just (f x)

instance Functor (Either e) where
  fmap f mx =
    case mx of
      Left e -> Left e
      Right x -> Right (f x)

instance Functor IO where
  fmap f (IO action) =
    IO
      ( \state ->
          case action state of
            (# nextState, value #) -> (# nextState, f value #)
      )

class (Functor f) => Applicative (f :: Type -> Type) where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

infixl 4 <*>

instance Applicative List where
  pure x = [x]

  fs <*> xs = applyList fs xs

instance Applicative Maybe where
  pure = Just

  mf <*> mx =
    case mf of
      Nothing -> Nothing
      Just f ->
        case mx of
          Nothing -> Nothing
          Just x -> Just (f x)

instance Applicative (Either e) where
  pure = Right

  mf <*> mx =
    case mf of
      Left e -> Left e
      Right f ->
        case mx of
          Left e -> Left e
          Right x -> Right (f x)

instance Applicative IO where
  pure value = IO (pureIO value)

  IO function <*> IO argument =
    IO
      ( \state ->
          case function state of
            (# functionState, f #) ->
              case argument functionState of
                (# resultState, value #) -> (# resultState, f value #)
      )

class (Applicative m) => Monad (m :: Type -> Type) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a

infixl 1 >>=, >>

(=<<) :: (Monad m) => (a -> m b) -> m a -> m b
f =<< mx = mx >>= f

infixr 1 =<<

instance Monad List where
  xs >>= k = bindList xs k

  xs >> ys = thenList xs ys
  return x = [x]

instance Monad Maybe where
  mx >>= k = bindMaybe mx k

  mx >> my =
    case mx of
      Nothing -> Nothing
      Just _ -> my
  return = Just

instance Monad (Either e) where
  mx >>= k =
    case mx of
      Left e -> Left e
      Right x -> k x

  mx >> my =
    case mx of
      Left e -> Left e
      Right _ -> my
  return = Right

instance Monad IO where
  IO action >>= k =
    IO
      ( \state ->
          case action state of
            (# nextState, value #) ->
              case k value of
                IO nextAction -> nextAction nextState
      )

  IO action >> IO nextAction =
    IO
      ( \state ->
          case action state of
            (# nextState, _ #) -> nextAction nextState
      )
  return = pure

pureIO :: a -> State# RealWorld -> (# State# RealWorld, a #)
pureIO value state = (# state, value #)

fmapList :: (a -> b) -> [a] -> [b]
fmapList _ [] = []
fmapList f (x : xs) = f x : fmapList f xs

applyList :: [a -> b] -> [a] -> [b]
applyList [] _ = []
applyList (f : fs) xs = fmapList f xs ++ applyList fs xs

bindList :: [a] -> (a -> [b]) -> [b]
bindList [] _ = []
bindList (x : xs) k = k x ++ bindList xs k

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing _ = Nothing
bindMaybe (Just x) k = k x

thenList :: [a] -> [b] -> [b]
thenList [] _ = []
thenList (_ : xs) ys = ys ++ thenList xs ys

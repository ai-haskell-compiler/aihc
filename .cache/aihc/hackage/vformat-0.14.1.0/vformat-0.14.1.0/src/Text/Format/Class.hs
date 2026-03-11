{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.Format.Class
  ( Formatter
  , FormatArg(..)
  , FromArgKey(..)
  , FormatType(..)
  , Options(..)
  , defaultOptions
  , genericFormatArg
  , (:=) (..)
  , formatString
  , formatChar
  , formatInt
  , formatWord
  , formatInteger
  , formatRealFloat
  , defaultSpecs
  ) where


import           Control.Monad.Catch
import           Data.Char
import           Data.Either
import           Data.Int
import           Data.Map            as M hiding (drop, map)
import           Data.Maybe
import           Data.Word
import           GHC.Generics
import           Numeric
import           Numeric.Natural

import           Text.Format.ArgFmt
import           Text.Format.ArgKey
import           Text.Format.Error
import           Text.Format.Format


type Formatter = ArgKey -> ArgFmt -> Either SomeException String


{-| Typeclass of formatable values.

The 'formatArg' method takes a value, a key and a field format descriptor and
either fails due to a 'ArgError' or  produce a string as the result.
There is a default 'formatArg' for 'Generic' instances, which applies
'defaultOptions' to 'genericFormatArg'.

There are two reasons may cause formatting fail

  (1) Can not find argument for the given key.

  (2) The field format descriptor does not match the argument.

==== Extending to new types

Those format functions can be extended to format types other than those
provided by default. This is done by instantiating 'FormatArg'.

Examples

@
\{\-\# LANGUAGE DeriveGeneric     \#\-\}
\{\-\# LANGUAGE OverloadedStrings \#\-\}

import           Control.Exception
import           GHC.Generics
import           Text.Format

-- Manually extend to ()
instance FormatArg () where
  formatArg x k fmt@(ArgFmt{fmtSpecs=\"U\"}) =
    let fmt' = fmt{fmtSpecs = \"\"}
    in  formatArg (show x) k fmt'
  formatArg _ _ _ = Left $ toException ArgFmtError

-- Use default generic implementation for type with nullary data constructors.
data Color = Red | Yellow | Blue deriving Generic

instance FormatArg Color

-- Use default generic implementation for type with non-nullary data constructor.
data Triple = Triple String Int Double deriving Generic

instance FormatArg Triple

-- Use default generic implementation for type using record syntax.
data Student = Student { no   :: Int
                       , name :: String
                       , age  :: Int
                       } deriving Generic

instance FormatArg Student

-- Customize field names
data Book = Book { bookName   :: String
                 , bookAuthor :: String
                 , bookPrice  :: Double
                 }

instance FormatArg Book where
  formatArg x k fmt
    | k == mempty = return $ format1 \"{name} {author} {price:.2f}\" x
    | k == Name \"name\" = formatArg (bookName x) mempty fmt
    | k == Name \"author\" = formatArg (bookAuthor x) mempty fmt
    | k == Name \"price\" = formatArg (bookPrice x) mempty fmt
    | otherwise = Left $ toException $ ArgKeyError

\-\- A better way to customize field names
\-\- instance FormatArg Book where
\-\-   formatArg = genericFormatArg $
\-\-     defaultOptions { fieldLabelModifier = drop 4 }

main :: IO ()
main = do
  putStrLn $ format \"A unit {:U}\" ()
  putStrLn $ format \"I like {}.\" Blue
  putStrLn $ format \"Triple {0!0} {0!1} {0!2}\" $ Triple \"Hello\" 123 pi
  putStrLn $ format1 \"Student: {no} {name} {age}\" $ Student 1 \"neo\" 30
  putStrLn $ format \"A book: {}\" $ Book \"Math\" \"nobody\" 99.99
  putStrLn $ format1 \"Book: {name}, Author: {author}, Price: {price:.2f}\" $
    Book \"Math\" \"nobody\" 99.99
@

Note: Since v0.12.0, 'FormatTime' instance has been remove,
use [vformat-time](http://hackage.haskell.org/package/vformat-time) instead.
-}
class FormatArg a where
  formatArg :: a -> Formatter

  default formatArg :: (Generic a, GFormatArg (Rep a)) => a -> Formatter
  formatArg = genericFormatArg defaultOptions

  formatArgList :: [a] -> Formatter
  formatArgList xs k | k == mempty = const $ throwM ArgKeyError
  formatArgList xs k | Just i <- fromArgKey (topKey k) =
    case drop (i - 1) xs of (x:_) -> formatArg x (popKey k)
  formatArgList  _ _ = const $ throwM ArgKeyError

  -- | This method is used to get the key of a top-level argument.
  -- Top-level argument means argument that directly passed to format
  -- functions ('format', 'format1').
  keyOf :: a -> ArgKey
  keyOf _ = mempty

instance FormatArg Bool

instance FormatArg Char where
  formatArg = formatChar
  formatArgList = formatString

instance FormatArg Float where
  formatArg = formatRealFloat

instance FormatArg Double where
  formatArg = formatRealFloat

instance FormatArg Int where
  formatArg = formatInt

instance FormatArg Int8 where
  formatArg = formatInt

instance FormatArg Int16 where
  formatArg = formatInt

instance FormatArg Int32 where
  formatArg = formatInt

instance FormatArg Int64 where
  formatArg = formatInt

instance FormatArg Integer where
  formatArg = formatInteger

instance FormatArg Natural where
  formatArg = formatIntegral False . toInteger

instance FormatArg Word where
  formatArg = formatWord

instance FormatArg Word8 where
  formatArg = formatWord

instance FormatArg Word16 where
  formatArg = formatWord

instance FormatArg Word32 where
  formatArg = formatWord

instance FormatArg Word64 where
  formatArg = formatWord

instance FormatArg a => FormatArg [a] where
  {-# SPECIALIZE instance FormatArg [Char] #-}
  formatArg = formatArgList

instance (Ord k, FromArgKey k, FormatArg v) => FormatArg (Map k v) where
  formatArg _ k | k == mempty = const $ throwM ArgKeyError
  formatArg x k | (Just k') <- fromArgKey (topKey k) =
    case (M.lookup k' x) of (Just x') -> formatArg x' (popKey k)
  formatArg _ _ = const $ throwM ArgKeyError


--------------------------------------------------------------------------------
{-| Typeclass for types that can be used as the key of a map-like or list-like
 container.

For example, since 'String' and 'Integral' datatypes are instance of
 'FromArgKey' and 'Double' is an instance of 'FormatArg', we can format a value
of type 'Map' 'String' 'Double'.

@since 0.13.0
-}
class FromArgKey a where
  fromArgKey :: ArgKey -> Maybe a

instance Integral k => FromArgKey k where
  fromArgKey (Index i) = Just $ toEnum i
  fromArgKey _         = Nothing

instance {-# OVERLAPPING #-} FromArgKey String where
  fromArgKey (Name k) = Just k
  fromArgKey _        = Nothing


--------------------------------------------------------------------------------
{-| Options that specify how to format your datatype

Options can be set using record syntax on defaultOptions with the fields below.

@since 0.11.0
-}
data Options = Options { fieldLabelModifier :: String -> String
                       }

{-| Default format options

@
'Options'
{ 'fieldLabelModifier' = id
}
@

@since 0.11.0
-}
defaultOptions = Options { fieldLabelModifier = id
                         }


{-| A configurable generic 'Formatter' creator.

@since 0.11.0
-}
genericFormatArg :: (Generic a, GFormatArg (Rep a)) => Options -> a -> Formatter
genericFormatArg opts x = gformatArg (from x) opts


--------------------------------------------------------------------------------
class GFormatArg f where
  gformatArg :: f p -> Options -> Formatter

-- Data type
instance GFormatArg f =>  GFormatArg (D1 c f) where
  gformatArg (M1 x) = gformatArg x

-- Choice between Sums
instance (GFormatArg f, GFormatArg g) => GFormatArg (f :+: g) where
  gformatArg (L1 x) = gformatArg x
  gformatArg (R1 x) = gformatArg x

-- Constructor
-- e.g. data GreetTo = Hello { name :: String } | Hi { name :: String }
--      data GreetTo = Hello String | Hi String
--      data Greet = Hello | Hi
instance (Constructor c, GFormatArg f) => GFormatArg (C1 c f) where
  gformatArg (M1 x) = gformatArg x

-- Constructor without arguments
-- e.g. data Greet = Hello | Hi
instance {-# OVERLAPPING  #-} Constructor c => GFormatArg (C1 c U1) where
  gformatArg c _ = formatArg (conName c)

-- Try Products one by one
instance (GFormatArg f, GFormatArg g) => GFormatArg (f :*: g) where
  gformatArg (x :*: y) opts k fmt =
      gformatArg x opts k fmt <|> gformatArg y opts (dec1 k) fmt
    where
      x <|> y = catchIf isArgKeyError x $ const y

      dec1 :: ArgKey -> ArgKey
      dec1 (Index i)    = Index (i - 1)
      dec1 (Nest k1 k2) = mappend (dec1 k1) k2
      dec1 k            = k

-- Selector (record and none record)
-- e.g. data GreetTo = Hello String | Hi String
--      data GreetTo = Hello { name :: String } | Hi { name :: String }
instance (Selector c, GFormatArg f) => GFormatArg (S1 c f) where
  gformatArg s@(M1 x) opts (Index 0)
    | selName s == "" = gformatArg x opts mempty
  gformatArg s@(M1 x) opts (Nest (Index 0) k)
    | selName s == "" = gformatArg x opts k
  gformatArg s@(M1 x) opts@(Options{..}) (Name record)
    | (fieldLabelModifier $ selName s) == record = gformatArg x opts mempty
  gformatArg s@(M1 x) opts@(Options{..}) (Nest (Name record) k)
    | (fieldLabelModifier $ selName s) == record = gformatArg x opts k
  gformatArg _ _ _ = const $ throwM ArgKeyError

-- FormatArg instance
instance (FormatArg c) => GFormatArg (K1 i c) where
  gformatArg (K1 c) _ = formatArg c


--------------------------------------------------------------------------------
-- | A typeclass provides the variable arguments magic for 'format'
--
class FormatType t where
  sfmt :: Format -> Map ArgKey Formatter -> t

instance (FormatArg a, FormatType r) => FormatType (a -> r) where
  sfmt fmt args = \arg -> sfmt fmt $
      insert (fixIndex $ keyOf arg) (formatArg arg) args
    where
      fixIndex k
        | k == mempty = Index $ length [n | Index n <- keys args]
        | otherwise   = k

instance FormatType String where
  sfmt fmt args = formats (unFormat fmt)
    where
      formats :: [FmtItem] -> String
      formats = concat . (map formats1)

      onError :: (ArgKey, ArgFmt) -> SomeException -> String
      onError (key, fmt) = catchArgError (errorArgKey $ show key)
                                         (errorArgFmt $ prettyArgFmt $ fmt)

      formats1 :: FmtItem -> String
      formats1 (Lit cs)       = cs
      formats1 (Arg key ifmt) = either (onError (key, ifmt)) id $
        (getFormatter key) (popKey key) (fixArgFmt ifmt)

      fixArgFmt :: ArgFmt -> ArgFmt
      fixArgFmt ifmt@(ArgFmt{fmtWidth=(Right key)}) =
        fixArgFmt $ ifmt {fmtWidth = Left $ formatWidth key}
      fixArgFmt ifmt@(ArgFmt{fmtPrecision=(Right key)}) =
        fixArgFmt $ ifmt {fmtPrecision = Left $ formatPrecision key}
      fixArgFmt ifmt = ifmt

      formatWidth, formatPrecision :: ArgKey -> Int
      formatWidth key =
        let fmt = read "0.0d"
        in read $ either (onError (key, fmt)) id $ (getFormatter key) key fmt
      formatPrecision = formatWidth

      getFormatter :: ArgKey -> Formatter
      getFormatter = maybe (\_ _ -> throwM ArgKeyError) id . (args !?) . topKey


--------------------------------------------------------------------------------
-- | A type represents the top-level named key argument.
data (:=) a = String := a
infixr 6 :=

instance FormatArg a => FormatArg ((:=) a) where
  formatArg (_ := x) k = formatArg x k
  keyOf (ks := _) = Name ks


--------------------------------------------------------------------------------
{-| Formatter for string values

@since 0.11.0
-}
formatString :: String -> Formatter
formatString _ k _ | k /= mempty = throwM ArgKeyError
formatString x _ fmt@(ArgFmt{fmtSpecs = ""})  = Right $ formatText fmt x
formatString x _ fmt@(ArgFmt{fmtSpecs = "s"}) = Right $ formatText fmt x
formatString _ _ _                            = throwM ArgFmtError


formatIntegral :: Bool -> Integer -> Formatter
formatIntegral _ _ k _ | k /= mempty = throwM ArgKeyError
formatIntegral signed x _ fmt@ArgFmt{fmtSpecs=specs} =
    formatNumber fmt signed (sepw specs) (flag specs) <$> (showx specs x)
  where
    sepw :: String -> Int
    sepw "b" = 4
    sepw "o" = 4
    sepw "x" = 4
    sepw "X" = 4
    sepw _   = 3

    flag :: String -> Maybe Char
    flag "b" = Just 'b'
    flag "o" = Just 'o'
    flag "x" = Just 'x'
    flag "X" = Just 'X'
    flag _   = Nothing

    encodeSign :: [Char] -> [Char]
    encodeSign "+" = "++"
    encodeSign "-" = "--"
    encodeSign cs  = cs

    showx :: String -> Integer -> Either SomeException String
    showx specs x
      | x < 0 = ('-' :) <$> showx specs (-x)
    showx "" x    = showx "d" x
    showx "b" x   = Right $ showIntAtBase 2 intToDigit x ""
    showx "c" x   = Right $ encodeSign $ [chr $ fromInteger x]
    showx "d" x   = Right $ show x
    showx "o" x   = Right $ showIntAtBase 8 intToDigit x ""
    showx "x" x   = Right $ showIntAtBase 16 intToDigit x ""
    showx "X" x   = map toUpper <$> showx "x" x
    showx _ _     = throwM ArgFmtError


{-| Formatter for 'Char' values

@since 0.11.0
-}
formatChar :: Char -> Formatter
formatChar = formatWord . ord

{-| Formatter for 'Int' values

@since 0.11.0
 -}
formatInt :: (Integral a, Bounded a) => a -> Formatter
formatInt = formatIntegral True . toInteger


{-| Formatter for 'Word' values

@since 0.11.0
 -}
formatWord :: (Integral a, Bounded a) => a -> Formatter
formatWord = formatIntegral False . toInteger


{-| Formatter for 'Integer' values

@since 0.11.0
 -}
formatInteger :: Integer -> Formatter
formatInteger = formatIntegral True


{-| Formatter for 'RealFloat' values

@since 0.11.0
 -}
formatRealFloat :: RealFloat a => a -> Formatter
formatRealFloat _ k _ | k /= mempty = throwM ArgKeyError
formatRealFloat x _ fmt@ArgFmt{fmtSpecs=specs, fmtPrecision=prec} =
    formatNumber fmt True 3 Nothing <$> showx specs prec1 x
  where
    prec1 = either (\i -> Just $ if i < 0 then 6 else i) (const $ Just 0) prec

    showx :: RealFloat a
          => String -> Maybe Int -> a -> Either SomeException String
    showx specs p x
      | x < 0 = ('-' :) <$> showx specs p (-x)
    showx "" p x    = showx "g" p x
    showx "e" p x   = Right $ showEFloat p x ""
    showx "E" p x   = map toUpper <$> showx "e" p x
    showx "f" p x   = Right $ showFFloat p x ""
    showx "F" p x   = map toUpper <$> showx "f" p x
    showx "g" p x   = Right $ showGFloat p x ""
    showx "G" p x   = map toUpper <$> showx "g" p x
    showx "%" p x   = (++ "%") <$> (showx "f" p (x * 100))
    showx _ _ _     = throwM ArgFmtError


{-| Use a default specs for the given formatter

@since 0.12.0
-}
defaultSpecs :: String -> (a -> Formatter) -> a -> Formatter
defaultSpecs specs f x k fmt
  | fmtSpecs fmt == "" = f x k $ fmt {fmtSpecs = specs}
  | otherwise = f x k fmt

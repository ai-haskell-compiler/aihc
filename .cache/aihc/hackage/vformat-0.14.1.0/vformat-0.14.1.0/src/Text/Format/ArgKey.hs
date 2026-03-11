{-# LANGUAGE CPP #-}

module Text.Format.ArgKey ( ArgKey (..), topKey, popKey ) where

import           Control.Arrow
import           Data.Char         (isDigit)
import qualified Data.List         as L
#if MIN_VERSION_base(4, 11, 0)
import           Data.Semigroup
#endif

import           Text.Format.Error


{-| A data type indicates key of format argument

==== The key syntax

  @
  key -> [(int | chars) {"!" (int | chars)}]
  @

  Since the "!" is used to seprate keys, if you need to include a "!" in a
  named key, it can be escaped by doubling "!!".

  Note: See 'Format' to learn more about syntax description language

  Examples

    >>> read "0" :: ArgKey
    >>> read "country" :: ArgKey
    >>> read "coun!!try" :: ArgKey
    >>> read "country!name" :: ArgKey
    >>> read "country!cities!10!name" :: ArgKey
-}
data ArgKey = Index Int           -- ^ Refers to a top-level positional
                                  -- argument or an element in  an list-like
                                  -- data type.
            | Name String         -- ^ Refers to a top-level named argument or
                                  -- a field of a record data type.
            | Nest ArgKey ArgKey  -- ^ For @Nest k1 k2@, k1 refers to a
                                  -- top-level argument or an attribute
                                  -- (element or field) of a data type,
                                  -- k2 refers an attribute of the data
                                  -- referenced by k1.
            deriving (Eq, Ord)

#if MIN_VERSION_base(4, 11, 0)
instance Semigroup ArgKey where
  (<>) = associate
#endif

instance Monoid ArgKey where
  -- | @Index -1@ is used as an empty key
  mempty = Index (-1)
#if !MIN_VERSION_base(4, 11, 0)
  mappend = associate
#endif

instance Read ArgKey where
  readsPrec _ "" = [ (mempty, "") ]
  readsPrec _ cs = [ parse cs ]
    where
      parse :: String -> (ArgKey, String)
      parse cs =
        case break cs of
          ("", cs1)  -> (undefined, cs1)
          (_, "!")   -> (undefined, "!")
          (cs1, "")  -> (parse1 cs1, "")
          (cs1, cs2) -> first (mappend $ parse1 cs1) (parse cs2)

      parse1 :: String -> ArgKey
      parse1 cs = if all isDigit cs then Index (read cs) else Name cs

      break :: String -> (String, String)
      break cs =
        case L.break (== '!') cs of
          (cs1, "")              -> (cs1, "")
          (cs1, "!")             -> (cs1, "!")
          (cs1, '!' : '!' : cs2) -> first ((cs1 ++ "!") ++) (break cs2)
          (cs1, '!' : cs2)       -> (cs1, cs2)

instance Show ArgKey where
  show k@(Index i) = if mempty == k then "" else show i
  show (Name s)    = escape s
    where
      escape :: String -> String
      escape ""         = ""
      escape ('!' : cs) = "!!" ++ escape cs
      escape (c : cs)   = (c : escape cs)
  show (Nest k1 k2)  = show k1 ++ "!" ++ show k2


associate :: ArgKey -> ArgKey -> ArgKey
associate k (Index (-1))    = k
associate (Index (-1)) k    = k
associate (Nest k11 k12) k2 = associate k11 $ associate k12 k2
associate k1 k2             = Nest k1 k2


{-| Extract the topmost indexed or named key from a key

>>> topKey (read "k1!k2!k3") == Name "k1"
True
>>> topKey (read "name") == Name "name"
True
>>> topKey (read "123") == Index 123
True
>>> topKey mempty
*** Exception: vformat: empty arg key
-}
topKey :: ArgKey -> ArgKey
topKey (Nest k@(Nest _ _) _) = topKey k
topKey (Nest k _) = k
topKey k = if k == mempty then vferror "empty arg key"
                          else k

{-| Remove the topmost indexed or named key from a key

>>> popKey (read "k1!k2!k3") == read "k2!k3"
True
>>> popKey (read "name") == mempty
True
>>> popKey (read "123") == mempty
True
>>> popKey mempty
*** Exception: vformat: empty arg key
-}
popKey :: ArgKey -> ArgKey
popKey (Nest k1@(Nest _ _) k2) = mappend (popKey k1) k2
popKey (Nest _ k) = k
popKey k = if k == mempty then vferror "empty arg key"
                          else mempty

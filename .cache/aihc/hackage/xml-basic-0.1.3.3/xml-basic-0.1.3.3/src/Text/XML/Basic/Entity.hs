module Text.XML.Basic.Entity (
   Name,
   list, listInternetExploder,
   mapNameToChar, mapCharToName,
   numberToChar,
   ) where

import qualified Data.Map as Map
import qualified Data.Char as Char
import Control.Monad.Exception.Synchronous (Exceptional, assert, throw, )
import Control.Monad.HT ((<=<), )
import Data.Monoid (Monoid(mempty, mappend), mconcat, )
import Data.Semigroup (Semigroup((<>)), )
import Data.Tuple.HT (swap, )


{- |
Lookup a numeric entity, the leading @\'#\'@ must have already been removed.

> numberToChar "65" == Success 'A'
> numberToChar "x41" == Success 'A'
> numberToChar "x4E" === Success 'N'
> numberToChar "x4e" === Success 'N'
> numberToChar "Haskell" == Exception "..."
> numberToChar "" == Exception "..."
> numberToChar "89439085908539082" == Exception "..."

It's safe to use that for arbitrary big number strings,
since we abort parsing as soon as possible.

> numberToChar (repeat '1') == Exception "..."
-}
numberToChar :: String -> Exceptional String Char
numberToChar s =
   fmap Char.chr $
   case s of
      ('x':ds) -> readBounded 16 Char.isHexDigit ds
      ds       -> readBounded 10 Char.isDigit    ds

{- |
We fail on too many leading zeros
in order to prevent infinite loop on @repeat '0'@.
This function assumes that @16 * ord maxBound@ is always representable as @Int@.
-}
readBounded :: Int -> (Char -> Bool) -> String -> Exceptional String Int
readBounded base validChar str =
   case str of
      ""  -> throw "empty number string"
      "0" -> return 0
      _ ->
         let m pos digit =
               Update $ \mostSig ->
                  let n = mostSig*base + Char.digitToInt digit
                  in  assert ("invalid character "++show digit)
                         (validChar digit) >>
                      assert "too many leading zeros forbidden in order to prevent denial of service"
                         (not (pos>=8 && digit=='0')) >>
                      assert "number too big"
                         (n <= Char.ord maxBound) >>
                      return n
         in  evalUpdate (mconcat $ zipWith m [(0::Int)..] str) 0


newtype Update e a = Update {evalUpdate :: a -> Exceptional e a}

instance Semigroup (Update e a) where
   Update x <> Update y = Update (y <=< x)

instance Monoid (Update e a) where
   mempty = Update return
   mappend = (<>)



type Name = String

mapNameToChar :: Map.Map Name Char
mapNameToChar =
   Map.fromList list

mapCharToName :: Map.Map Char Name
mapCharToName =
   Map.fromList $ map swap list

{- |
A table mapping XML entity names to code points.
Although entity references can in principle represent more than one character,
the standard entities only contain one character.
-}
list :: [(Name, Char)]
list =
   ("apos",   '\'') :
   listInternetExploder

{- |
This list excludes @apos@ as Internet Explorer does not know about it.
-}
listInternetExploder :: [(Name, Char)]
listInternetExploder =
   ("quot",   '"') :
   ("amp",    '&') :
   ("lt",     '<') :
   ("gt",     '>') :
   []

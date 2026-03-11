{- |
Support for creating Show instances using the accessors.
-}
module Data.Accessor.Show (field, showsPrec) where

import qualified Data.Accessor.Basic as Accessor

import Data.Maybe (catMaybes)

-- import qualified Text.Show as Show
import qualified Prelude as Show
import Prelude hiding (showsPrec)


toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True  x = Just x

field :: (Show a, Eq a) =>
   String -> Accessor.T r a -> r -> r -> Maybe ShowS
field name acc deflt record =
   let x = Accessor.get acc record
   in  toMaybe
          (x /= Accessor.get acc deflt)
          (showString name . showString " ^= " . Show.showsPrec 5 x)

showsPrec ::
   [r -> r -> Maybe ShowS] -> String -> r -> Int -> r -> ShowS
showsPrec fields defltName deflt p record =
   let calls =
          catMaybes $
          map (\f -> f deflt record) $
          fields
   in  if null calls
         then showString defltName
         else showParen (p>0)
                 (foldr
                     (\acc s -> acc . showString " $ " . s)
                     (showString defltName)
                     calls)

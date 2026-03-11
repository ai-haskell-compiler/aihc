module Sound.Sox.Private.Option where

import qualified Sound.Sox.Private.Arguments as Args
import qualified Data.Map as Map
import Data.Monoid (Monoid(mappend, mempty), )
import Data.Semigroup (Semigroup((<>)), )

{- |
You can combine options using the 'Monoid' functions 'mappend' and 'mconcat'.
When the same option is given multiple times,
only the first occurence is respected.
-}
newtype T = Cons {decons :: Map.Map String [String]}

instance Semigroup T where
   Cons x <> Cons y = Cons (Map.union x y)

instance Monoid T where
   mempty = Cons mempty
   mappend = (<>)

toArguments :: T -> Args.T
toArguments =
   Args.Cons .
   concatMap (\(name,values) -> name:values) . Map.toList .
   decons

single :: String -> [String] -> T
single name values =
   Cons (Map.singleton name values)

module Sound.Sox.Private.Arguments where

import Data.Monoid (Monoid(mempty, mappend), )
import Data.Semigroup (Semigroup((<>)), )

newtype T = Cons {decons :: [String]}

instance Semigroup T where
   Cons x <> Cons y = Cons (x++y)

instance Monoid T where
   mempty = Cons mempty
   mappend = (<>)

single :: String -> T
single x = Cons [x]

fileName :: FilePath -> T
fileName = single

pipe :: T
pipe = single "-"

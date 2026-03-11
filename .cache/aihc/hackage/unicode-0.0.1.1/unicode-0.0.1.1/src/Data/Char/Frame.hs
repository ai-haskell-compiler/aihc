module Data.Char.Frame where

import Control.Applicative (Applicative, pure, (<*>), liftA2, )
import Data.Traversable (Traversable, traverse, foldMapDefault, )
import Data.Foldable (Foldable, foldMap, )
import Data.Monoid (Monoid, mempty, mappend, )
import Data.Semigroup (Semigroup((<>)), )


data Horizontal a = Horizontal {left, right :: a} deriving (Eq, Show)
data Vertical a = Vertical {up, down :: a} deriving (Eq, Show)
data Parts a = Parts (Vertical a) (Horizontal a) deriving (Eq, Show)

instance Semigroup a => Semigroup (Horizontal a) where
   Horizontal xl xr <> Horizontal yl yr =
      Horizontal (xl <> yl) (xr <> yr)

instance Monoid a => Monoid (Horizontal a) where
   mempty = Horizontal mempty mempty
   mappend (Horizontal xl xr) (Horizontal yl yr) =
      Horizontal (mappend xl yl) (mappend xr yr)

instance Semigroup a => Semigroup (Vertical a) where
   Vertical xl xr <> Vertical yl yr =
      Vertical (xl <> yl) (xr <> yr)

instance Monoid a => Monoid (Vertical a) where
   mempty = Vertical mempty mempty
   mappend (Vertical xl xr) (Vertical yl yr) =
      Vertical (mappend xl yl) (mappend xr yr)

instance Semigroup a => Semigroup (Parts a) where
   Parts xl xr <> Parts yl yr =
      Parts (xl <> yl) (xr <> yr)

instance Monoid a => Monoid (Parts a) where
   mempty = Parts mempty mempty
   mappend (Parts xl xr) (Parts yl yr) =
      Parts (mappend xl yl) (mappend xr yr)


instance Functor Horizontal where
   fmap f (Horizontal a b) = Horizontal (f a) (f b)

instance Functor Vertical where
   fmap f (Vertical a b) = Vertical (f a) (f b)

instance Functor Parts where
   fmap f (Parts a b) = Parts (fmap f a) (fmap f b)


instance Foldable Horizontal where
   foldMap = foldMapDefault

instance Foldable Vertical where
   foldMap = foldMapDefault

instance Foldable Parts where
   foldMap = foldMapDefault


instance Traversable Horizontal where
   traverse f (Horizontal a b) = liftA2 Horizontal (f a) (f b)

instance Traversable Vertical where
   traverse f (Vertical a b) = liftA2 Vertical (f a) (f b)

instance Traversable Parts where
   traverse f (Parts a b) = liftA2 Parts (traverse f a) (traverse f b)


instance Applicative Horizontal where
   pure a = Horizontal a a
   Horizontal fa fb <*> Horizontal a b =
      Horizontal (fa a) (fb b)

instance Applicative Vertical where
   pure a = Vertical a a
   Vertical fa fb <*> Vertical a b =
      Vertical (fa a) (fb b)

instance Applicative Parts where
   pure a = Parts (pure a) (pure a)
   Parts fa fb <*> Parts a b =
      Parts (fa <*> a) (fb <*> b)


simple :: Parts Bool -> Char
simple set =
   case set of
      Parts (Vertical False False) (Horizontal False False) -> ' '
      Parts (Vertical False False) (Horizontal True  True ) -> '\x2500'
      Parts (Vertical True  True ) (Horizontal False False) -> '\x2502'
      Parts (Vertical True  True ) (Horizontal True  True ) -> '\x253C'

      Parts (Vertical False False) (Horizontal False True ) -> '\x2576'
      Parts (Vertical False False) (Horizontal True  False) -> '\x2574'
      Parts (Vertical False True ) (Horizontal False False) -> '\x2577'
      Parts (Vertical True  False) (Horizontal False False) -> '\x2575'

      Parts (Vertical False True ) (Horizontal False True ) -> '\x250C'
      Parts (Vertical False True ) (Horizontal True  False) -> '\x2510'
      Parts (Vertical True  False) (Horizontal False True ) -> '\x2514'
      Parts (Vertical True  False) (Horizontal True  False) -> '\x2518'

      Parts (Vertical True  True ) (Horizontal False True ) -> '\x251C'
      Parts (Vertical True  True ) (Horizontal True  False) -> '\x2524'
      Parts (Vertical False True ) (Horizontal True  True ) -> '\x252C'
      Parts (Vertical True  False) (Horizontal True  True ) -> '\x2534'


data Weight = Empty | Light | Heavy
   deriving (Eq, Ord, Show, Enum, Bounded)

weighted :: Parts Weight -> Char
weighted set =
   case set of
      Parts (Vertical Empty Empty) (Horizontal Empty Empty) -> ' '
      Parts (Vertical Empty Empty) (Horizontal Light Light) -> '\x2500'
      Parts (Vertical Empty Empty) (Horizontal Heavy Heavy) -> '\x2501'
      Parts (Vertical Light Light) (Horizontal Empty Empty) -> '\x2502'
      Parts (Vertical Heavy Heavy) (Horizontal Empty Empty) -> '\x2503'
      Parts (Vertical Empty Light) (Horizontal Empty Light) -> '\x250C'
      Parts (Vertical Empty Light) (Horizontal Empty Heavy) -> '\x250D'
      Parts (Vertical Empty Heavy) (Horizontal Empty Light) -> '\x250E'
      Parts (Vertical Empty Heavy) (Horizontal Empty Heavy) -> '\x250F'
      Parts (Vertical Empty Light) (Horizontal Light Empty) -> '\x2510'
      Parts (Vertical Empty Light) (Horizontal Heavy Empty) -> '\x2511'
      Parts (Vertical Empty Heavy) (Horizontal Light Empty) -> '\x2512'
      Parts (Vertical Empty Heavy) (Horizontal Heavy Empty) -> '\x2513'
      Parts (Vertical Light Empty) (Horizontal Empty Light) -> '\x2514'
      Parts (Vertical Light Empty) (Horizontal Empty Heavy) -> '\x2515'
      Parts (Vertical Heavy Empty) (Horizontal Empty Light) -> '\x2516'
      Parts (Vertical Heavy Empty) (Horizontal Empty Heavy) -> '\x2517'
      Parts (Vertical Light Empty) (Horizontal Light Empty) -> '\x2518'
      Parts (Vertical Light Empty) (Horizontal Heavy Empty) -> '\x2519'
      Parts (Vertical Heavy Empty) (Horizontal Light Empty) -> '\x251A'
      Parts (Vertical Heavy Empty) (Horizontal Heavy Empty) -> '\x251B'
      Parts (Vertical Light Light) (Horizontal Empty Light) -> '\x251C'
      Parts (Vertical Light Light) (Horizontal Empty Heavy) -> '\x251D'
      Parts (Vertical Heavy Light) (Horizontal Empty Light) -> '\x251E'
      Parts (Vertical Light Heavy) (Horizontal Empty Light) -> '\x251F'
      Parts (Vertical Heavy Heavy) (Horizontal Empty Light) -> '\x2520'
      Parts (Vertical Heavy Light) (Horizontal Empty Heavy) -> '\x2521'
      Parts (Vertical Light Heavy) (Horizontal Empty Heavy) -> '\x2522'
      Parts (Vertical Heavy Heavy) (Horizontal Empty Heavy) -> '\x2523'
      Parts (Vertical Light Light) (Horizontal Light Empty) -> '\x2524'
      Parts (Vertical Light Light) (Horizontal Heavy Empty) -> '\x2525'
      Parts (Vertical Heavy Light) (Horizontal Light Empty) -> '\x2526'
      Parts (Vertical Light Heavy) (Horizontal Light Empty) -> '\x2527'
      Parts (Vertical Heavy Heavy) (Horizontal Light Empty) -> '\x2528'
      Parts (Vertical Heavy Light) (Horizontal Heavy Empty) -> '\x2529'
      Parts (Vertical Light Heavy) (Horizontal Heavy Empty) -> '\x252A'
      Parts (Vertical Heavy Heavy) (Horizontal Heavy Empty) -> '\x252B'
      Parts (Vertical Empty Light) (Horizontal Light Light) -> '\x252C'
      Parts (Vertical Empty Light) (Horizontal Heavy Light) -> '\x252D'
      Parts (Vertical Empty Light) (Horizontal Light Heavy) -> '\x252E'
      Parts (Vertical Empty Light) (Horizontal Heavy Heavy) -> '\x252F'
      Parts (Vertical Empty Heavy) (Horizontal Light Light) -> '\x2530'
      Parts (Vertical Empty Heavy) (Horizontal Heavy Light) -> '\x2531'
      Parts (Vertical Empty Heavy) (Horizontal Light Heavy) -> '\x2532'
      Parts (Vertical Empty Heavy) (Horizontal Heavy Heavy) -> '\x2533'
      Parts (Vertical Light Empty) (Horizontal Light Light) -> '\x2534'
      Parts (Vertical Light Empty) (Horizontal Heavy Light) -> '\x2535'
      Parts (Vertical Light Empty) (Horizontal Light Heavy) -> '\x2536'
      Parts (Vertical Light Empty) (Horizontal Heavy Heavy) -> '\x2537'
      Parts (Vertical Heavy Empty) (Horizontal Light Light) -> '\x2538'
      Parts (Vertical Heavy Empty) (Horizontal Heavy Light) -> '\x2539'
      Parts (Vertical Heavy Empty) (Horizontal Light Heavy) -> '\x253A'
      Parts (Vertical Heavy Empty) (Horizontal Heavy Heavy) -> '\x253B'
      Parts (Vertical Light Light) (Horizontal Light Light) -> '\x253C'
      Parts (Vertical Light Light) (Horizontal Heavy Light) -> '\x253D'
      Parts (Vertical Light Light) (Horizontal Light Heavy) -> '\x253E'
      Parts (Vertical Light Light) (Horizontal Heavy Heavy) -> '\x253F'
      Parts (Vertical Heavy Light) (Horizontal Light Light) -> '\x2540'
      Parts (Vertical Light Heavy) (Horizontal Light Light) -> '\x2541'
      Parts (Vertical Heavy Heavy) (Horizontal Light Light) -> '\x2542'
      Parts (Vertical Heavy Light) (Horizontal Heavy Light) -> '\x2543'
      Parts (Vertical Heavy Light) (Horizontal Light Heavy) -> '\x2544'
      Parts (Vertical Light Heavy) (Horizontal Heavy Light) -> '\x2545'
      Parts (Vertical Light Heavy) (Horizontal Light Heavy) -> '\x2546'
      Parts (Vertical Heavy Light) (Horizontal Heavy Heavy) -> '\x2547'
      Parts (Vertical Light Heavy) (Horizontal Heavy Heavy) -> '\x2548'
      Parts (Vertical Heavy Heavy) (Horizontal Heavy Light) -> '\x2549'
      Parts (Vertical Heavy Heavy) (Horizontal Light Heavy) -> '\x254A'
      Parts (Vertical Heavy Heavy) (Horizontal Heavy Heavy) -> '\x254B'
      Parts (Vertical Empty Empty) (Horizontal Light Empty) -> '\x2574'
      Parts (Vertical Light Empty) (Horizontal Empty Empty) -> '\x2575'
      Parts (Vertical Empty Empty) (Horizontal Empty Light) -> '\x2576'
      Parts (Vertical Empty Light) (Horizontal Empty Empty) -> '\x2577'
      Parts (Vertical Empty Empty) (Horizontal Heavy Empty) -> '\x2578'
      Parts (Vertical Heavy Empty) (Horizontal Empty Empty) -> '\x2579'
      Parts (Vertical Empty Empty) (Horizontal Empty Heavy) -> '\x257A'
      Parts (Vertical Empty Heavy) (Horizontal Empty Empty) -> '\x257B'
      Parts (Vertical Empty Empty) (Horizontal Light Heavy) -> '\x257C'
      Parts (Vertical Light Heavy) (Horizontal Empty Empty) -> '\x257D'
      Parts (Vertical Empty Empty) (Horizontal Heavy Light) -> '\x257E'
      Parts (Vertical Heavy Light) (Horizontal Empty Empty) -> '\x257F'


data Directions a = Directions {vertical, horizontal :: a} deriving (Eq, Show)

instance Functor Directions where
   fmap f (Directions a b) = Directions (f a) (f b)

instance Foldable Directions where
   foldMap = foldMapDefault

instance Traversable Directions where
   traverse f (Directions a b) = liftA2 Directions (f a) (f b)

instance Applicative Directions where
   pure a = Directions a a
   Directions fa fb <*> Directions a b =
      Directions (fa a) (fb b)


{- |
This function is not total because half-width and half-height double bars are missing.
-}
double :: Directions Bool -> Parts Bool -> Char
double doubled set =
   maybe (error "Frame.double: frame character not available") id $
   doubleMaybe doubled set

doubleMaybe :: Directions Bool -> Parts Bool -> Maybe Char
doubleMaybe doubled set =
   let adapt base =
          Just $
          case doubled of
             Directions False False -> simple set
             Directions False True -> base
             Directions True False -> succ base
             Directions True True -> succ $ succ base
   in  case (doubled, set) of
          (Directions _ _,     Parts (Vertical False False) (Horizontal False False)) -> Just ' '
          (Directions _ False, Parts (Vertical False False) (Horizontal True  True )) -> Just '\x2500'
          (Directions False _, Parts (Vertical True  True ) (Horizontal False False)) -> Just '\x2502'

          (Directions _ True, Parts (Vertical False False) (Horizontal True  True )) -> Just '\x2550'
          (Directions True _, Parts (Vertical True  True ) (Horizontal False False)) -> Just '\x2551'

          (Directions _ False, Parts (Vertical False False) (Horizontal True  False)) -> Just '\x2574'
          (Directions False _, Parts (Vertical True  False) (Horizontal False False)) -> Just '\x2575'
          (Directions _ False, Parts (Vertical False False) (Horizontal False True )) -> Just '\x2576'
          (Directions False _, Parts (Vertical False True ) (Horizontal False False)) -> Just '\x2577'

          (Directions _ True, Parts (Vertical False False) (Horizontal False True )) -> Nothing
          (Directions True _, Parts (Vertical True  False) (Horizontal False False)) -> Nothing
          (Directions _ True, Parts (Vertical False False) (Horizontal True  False)) -> Nothing
          (Directions True _, Parts (Vertical False True ) (Horizontal False False)) -> Nothing

          (_, Parts (Vertical False True) (Horizontal False True)) -> adapt '\x2552'
          (_, Parts (Vertical False True) (Horizontal True False)) -> adapt '\x2555'
          (_, Parts (Vertical True False) (Horizontal False True)) -> adapt '\x2558'
          (_, Parts (Vertical True False) (Horizontal True False)) -> adapt '\x255B'
          (_, Parts (Vertical True True) (Horizontal False True)) -> adapt '\x255E'
          (_, Parts (Vertical True True) (Horizontal True False)) -> adapt '\x2561'
          (_, Parts (Vertical False True) (Horizontal True True)) -> adapt '\x2564'
          (_, Parts (Vertical True False) (Horizontal True True)) -> adapt '\x2567'
          (_, Parts (Vertical True True) (Horizontal True True)) -> adapt '\x256A'


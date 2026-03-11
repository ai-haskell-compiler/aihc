{-# LANGUAGE
  GeneralizedNewtypeDeriving,
  TypeFamilies #-}

-- | Line-column locations and its offset monoid.
module DiffLoc.Colline
  ( Colline(..)
  , Vallee(..)
  , Vallée
  ) where

import Data.Functor ((<&>))
import DiffLoc.Shift

-- $setup
-- >>> import Test.QuickCheck
-- >>> import DiffLoc
-- >>> import DiffLoc.Test
-- >>> import DiffLoc.Unsafe ((.-.))

-- | Line and column coordinates.
--
-- The generalization over types of line and column numbers
-- frees us from any specific indexing scheme, notably whether
-- columns are zero- or one-indexed.
--
-- === Example
--
-- > abc
-- > de
-- > fgh
--
-- Assuming the lines and columns are both 1-indexed, @"b"@ is at location
-- @(Colline 1 2)@ and @"h"@ is at location @(Colline 3 3)@.
data Colline l c = Colline !l !c
  deriving (Eq, Ord, Show)

-- | The space between two 'Colline's.
--
-- This type represents offsets between text locations @x <= y@
-- as the number of newlines inbetween and the number of characters
-- from the last new line to @y@, if there is at least one newline,
-- or the number of characters from @x@ to @y@.
--
-- === Example
--
-- > abc
-- > de
-- > fgh
--
-- - The offset from @"b"@ to @"h"@ is @Vallee 2 2@ (two newlines to reach line 3,
--   and from the beginning of that line, advance two characters to reach h).
-- - The offset from @"b"@ to @"c"@ is @Vallee 0 1@ (advance one character).
--
-- The offset from @"b"@ to @"h"@ is actually the same as from @"a"@ to @"h"@
-- and from @"c"@ to @"h"@. Line-column offsets are thus not invertible.
-- This was one of the main constraints in the design of the 'Amor' class.
data Vallee dl dc = Vallee !dl !dc
  deriving (Eq, Ord, Show)

-- | Sans commentaire.
type Vallée = Vallee

-- $hidden
-- prop> (x <> y) <> z === x <> (y <> z :: Vallee (Offset Int) (Offset Int))

traversee ::
  Eq dl =>
  dl ->
  (l -> dl -> l) ->
  (c -> dc -> c) ->
  (dc -> c) ->
  Colline l c -> Vallee dl dc -> Colline l c
traversee zero actL actC fromO (Colline l c) (Vallee l' c')
  | l' == zero = Colline l (c `actC` c')
  | otherwise = Colline (l `actL` l') (fromO c')

instance (Monoid l, Eq l, Semigroup c) => Semigroup (Vallee l c) where
  x <> y = descente (traversee mempty (<>) (<>) id (montee x) y)
    where
      montee :: Vallee l c -> Colline l c
      montee (Vallee l c) = Colline l c

      descente :: Colline l c -> Vallee l c
      descente (Colline l c) = Vallee l c

instance (Monoid l, Eq l, Monoid c) => Monoid (Vallee l c) where
  mempty = Vallee mempty mempty

-- $hidden
-- prop> (i .+ r) .+ s === (i .+ (r <> s) :: Colline N N')
-- prop> i <= j ==> (i .+ (j .-. i)) === (j :: Colline N N')
-- prop> (i .+ r) .-. (i :: Colline N N') === r

instance (Amor l, Origin c) => Amor (Colline l c) where
  type Trans (Colline l c) = Vallee (Trans l) (Trans c)

  (.+) = traversee mempty (.+) (.+) ofOrigin

  Colline l c .-.? Colline l' c' = case compare l l' of
    LT -> Nothing
    EQ | c' <= c -> Vallee mempty <$> (c .-.? c')
       | otherwise -> Nothing
    GT -> (l .-.? l') <&> \dl -> Vallee dl (fromOrigin c)

instance (Origin l, Origin c) => Origin (Colline l c) where
  origin = Colline origin origin

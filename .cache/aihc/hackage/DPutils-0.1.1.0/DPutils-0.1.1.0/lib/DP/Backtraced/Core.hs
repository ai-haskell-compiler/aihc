
-- | The base constructors for generic backtracing.
--
-- NOTE this currently can capture dot-bracket notation, but not deep
-- semantics.

module DP.Backtraced.Core where

import           Control.Lens
import           Data.Foldable
import           GHC.Generics (Generic)
import qualified Data.Sequence as Seq
import qualified Test.QuickCheck as QC
import qualified Test.SmallCheck.Series as SC

-- | This is a bit like a lazy "Data.Sequence" in terms of constructors. We can
-- not be spine-strict, otherwise we'd use @Data.Sequence@ and enjoy the better
-- performance.

data Backtraced ty where
  -- | This backtrace is done
  Epsilon ∷ Backtraced ty
  -- | Expand a backtrace to the left. This is lazy, since backtracing relies
  -- on laziness.
  Cons ∷ ty → Backtraced ty → Backtraced ty
  -- | Expand lazily to the right.
  Snoc ∷ Backtraced ty → ty → Backtraced ty
  -- | concatenate two structures
  Append ∷ Backtraced ty → Backtraced ty → Backtraced ty
  deriving (Eq,Ord,Show,Read,Generic,Functor,Foldable,Traversable)

-- | This is somewhat tricky, since we might have to walk down the structure
-- quite a bit and shuffle constructors without changing the actual leaf order.

instance Cons (Backtraced ty) (Backtraced ty') ty ty' where
  {-# Inlinable _Cons #-}
  _Cons =
    let go1 Epsilon = Left Epsilon
        go1 (Cons x xs)    = Right (x,xs)
        go1 (Snoc xs x)    = go2 xs (Left x)
        go1 (Append xs ys) = go2 xs (Right ys)
        go2 Epsilon        (Left y)   = Right (y,Epsilon)
        go2 Epsilon        (Right ys) = go1 ys
        go2 (Cons x xs)    (Left y)   = Right (x,Snoc xs y)
        go2 (Cons x xs)    (Right ys) = Right (x, Append xs ys)
        go2 (Snoc xs x)    (Left y)   = go2 xs (Right $ x `Cons` Epsilon `Snoc` y)
        go2 (Snoc xs x)    (Right ys) = go2 xs (Right $ x `Cons` ys)
        go2 (Append xs ys) (Left z)   = go2 xs (Right $ ys `Snoc` z)
        go2 (Append xs ys) (Right zs) = go2 xs (Right $ ys `Append` zs)
    in  prism (uncurry Cons) go1

instance Snoc (Backtraced ty) (Backtraced ty') ty ty' where
  {-# Inlinable _Snoc #-}
  _Snoc =
    let go1 Epsilon = Left Epsilon
        go1 (Cons x xs)    = go2 (Left x) xs
        go1 (Snoc xs x)    = Right (xs,x)
        go1 (Append xs ys) = go2 (Right xs) ys
        go2 (Left x)   Epsilon        = Right (Epsilon,x)
        go2 (Right xs) Epsilon        = go1 xs
        go2 (Left x)   (Cons y ys)    = go2 (Right $ x `Cons` (y `Cons` Epsilon)) ys
        go2 (Right xs) (Cons y ys)    = go2 (Right $ xs `Snoc` y) ys
        go2 (Left x)   (Snoc ys y)    = Right (x `Cons` ys, y)
        go2 (Right xs) (Snoc ys y)    = Right (xs `Append` ys, y)
        go2 (Left x)   (Append ys zs) = go2 (Right $ x `Cons` ys) zs
        go2 (Right xs) (Append ys zs) = go2 (Right $ xs `Append` ys) zs
    in  prism (uncurry Snoc) go1

(<|) = Cons
(|>) = Snoc
(><) = Append

infixr 5 <|
infixr 5 ><
infixl 5 |>

instance SC.Serial m a ⇒ SC.Serial m (Backtraced a)


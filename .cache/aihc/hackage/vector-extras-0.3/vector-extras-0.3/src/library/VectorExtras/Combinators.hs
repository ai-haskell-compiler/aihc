-- |
-- Vector-specialised combinators often used for parsing.
module VectorExtras.Combinators where

import Base.Prelude
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as Vector
import qualified VectorExtras.Accumulator as Acc
import VectorExtras.Combinators.Helpers

-- * Common

many :: (MonadPlus m, Vector v a) => m a -> m (v a)
many getElement =
  accMany Acc.init getElement <&> Acc.toVector

sepBy :: (MonadPlus m, Vector v a) => m a -> m sep -> m (v a)
sepBy = flip sep

sepBy1 :: (MonadPlus m, Vector v a) => m a -> m sep -> m (v a)
sepBy1 = flip sep1

-- |
-- Same as 'sepBy' but with arguments flipped.
-- Because the order of arguments in 'sepBy' does not feel right.
sep :: (MonadPlus m, Vector v a) => m sep -> m a -> m (v a)
sep getSeparator getElement =
  orPure Vector.empty $ sep1 getSeparator getElement

-- |
-- Same as 'sepBy1' but with arguments flipped.
-- Because the order of arguments in 'sepBy1' does not feel right.
sep1 :: (MonadPlus m, Vector v a) => m sep -> m a -> m (v a)
sep1 getSeparator getElement =
  do
    element <- getElement
    Acc.toVector <$> accMany (Acc.add element Acc.init) (getSeparator *> getElement)

sepEnd :: (MonadPlus m, Vector v a) => m sep -> m end -> m a -> m (v a)
sepEnd getSeparator getEnd getElement =
  orPure Vector.empty $ sepEnd1 getSeparator getEnd getElement

sepEnd1 :: (MonadPlus m, Vector v a) => m sep -> m end -> m a -> m (v a)
sepEnd1 getSeparator getEnd getElement =
  do
    element <- getElement
    acc <- collect (Acc.add element Acc.init)
    return $ Acc.toVector acc
  where
    collect !acc =
      getEnd $> acc <|> (getSeparator >> getElement >>= \e -> collect (Acc.add e acc))

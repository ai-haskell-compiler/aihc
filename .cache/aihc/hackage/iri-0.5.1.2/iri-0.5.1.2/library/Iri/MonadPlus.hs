module Iri.MonadPlus where

import Iri.Prelude hiding (foldl)
import qualified Ptr.ByteString as ByteString
import qualified Ptr.Poking as Poking

{-# INLINE foldl #-}
foldl :: (MonadPlus m) => (a -> b -> a) -> a -> m b -> m a
foldl step start elementParser =
  loop start
  where
    loop state =
      mplus
        ( do
            element <- elementParser
            loop $! step state element
        )
        (return state)

{-# INLINE foldlM #-}
foldlM :: (MonadPlus m) => (a -> b -> m a) -> a -> m b -> m a
foldlM step start elementParser =
  loop start
  where
    loop state =
      join
        ( mplus
            ( do
                element <- elementParser
                return (step state element >>= loop)
            )
            (return (return state))
        )

{-# INLINE foldByteString #-}
foldByteString :: (MonadPlus m) => m ByteString -> m ByteString
foldByteString =
  fmap ByteString.poking
    . foldl (\poking -> mappend poking . Poking.bytes) mempty

{-# INLINE fold #-}
fold :: (MonadPlus m, Monoid a) => m a -> m a
fold = foldl mappend mempty

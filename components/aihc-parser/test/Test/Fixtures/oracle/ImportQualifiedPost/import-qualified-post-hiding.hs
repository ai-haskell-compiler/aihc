{- ORACLE_TEST pass -}
{-# LANGUAGE ImportQualifiedPost #-}

module ImportQualifiedPostHiding where

import Prelude qualified as P hiding (mapM)

liftMaybe :: a -> Maybe a
liftMaybe = P.pure
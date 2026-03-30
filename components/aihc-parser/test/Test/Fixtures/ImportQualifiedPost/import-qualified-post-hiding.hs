{- ORACLE_TEST
id: import-qualified-post-hiding
category: modules
expected: pass
-}
{-# LANGUAGE ImportQualifiedPost #-}

module ImportQualifiedPostHiding where

import Prelude qualified as P hiding (mapM)

liftMaybe :: a -> Maybe a
liftMaybe = P.pure

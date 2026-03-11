{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP, TemplateHaskell #-}
module GHC.Check.Util (MyVersion(..), liftTyped, gcatchSafe) where

import           Control.Exception.Safe as Safe
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Data.Version ( Version, parseVersion )
import           GHC (Ghc)
import qualified GHC
import           GHC.Exts                   (IsList (fromList), toList)
import           Language.Haskell.TH ( TExpQ )
import           Language.Haskell.TH.Syntax as TH
import           Language.Haskell.TH.Syntax.Compat
import qualified Text.Read as Read

-- | A wrapper around 'Version' with TH lifting
newtype MyVersion = MyVersion Version
  deriving (Eq, IsList, Show)

instance Lift MyVersion where
#if MIN_VERSION_template_haskell(2,16,0)
    liftTyped = liftMyVersion
#else
    lift = unTypeQ . liftMyVersion
#endif
    -- lift = unTypeCode . liftMyVersion

instance Read MyVersion where
  readPrec = Read.lift $ MyVersion <$> parseVersion

#if MIN_VERSION_template_haskell(2,17,0)
liftMyVersion :: (Quote m) => MyVersion -> Splice m MyVersion
#else
liftMyVersion :: MyVersion -> TExpQ MyVersion
#endif
liftMyVersion ver = liftSplice $ do
    verLifted <- liftQuote (toList ver)
    examineSplice [|| fromList $$( liftSplice . pure $ TExp verLifted)||]


#if !MIN_VERSION_template_haskell(2,16,0)
liftTyped :: Lift a => a -> TExpQ a
liftTyped = unsafeTExpCoerce . lift
#endif

gcatchSafe :: forall e a . Exception e => Ghc a -> (e -> Ghc a) -> Ghc a
#if MIN_VERSION_ghc(9,0,1)
gcatchSafe = Safe.catch
#else
gcatchSafe act h = act `GHC.gcatch` rethrowAsyncExceptions
  where
      rethrowAsyncExceptions :: e -> Ghc a
      rethrowAsyncExceptions e
        | isAsyncException e = liftIO . throwIO $ e
        | otherwise = h e
#endif

{-# LANGUAGE ScopedTypeVariables, StandaloneDeriving, TemplateHaskell #-}
module Debian.Loc
    ( __LOC__
    , mapExn
    ) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Exception (Exception, throw)
import Control.Monad.Catch (MonadCatch, catch)
import Language.Haskell.TH

__LOC__ :: Q Exp
__LOC__ = location >>= \ x ->
             recConE 'Loc [ (,) <$> (pure 'loc_filename) <*> litE (stringL (loc_filename x))
                          , (,) <$> (pure 'loc_package) <*> litE (stringL (loc_package x))
                          , (,) <$> (pure 'loc_module) <*> litE (stringL (loc_module x))
                          , (,) <$> (pure 'loc_start) <*> [|($(litE (integerL (fromIntegral (fst (loc_start x))))),
                                                             $(litE (integerL (fromIntegral (snd (loc_start x)))))) :: (Int, Int)|]
                          , (,) <$> (pure 'loc_end) <*> [|($(litE (integerL (fromIntegral (fst (loc_end x))))),
                                                           $(litE (integerL (fromIntegral (snd (loc_end x)))))) :: (Int, Int)|] ]

mapExn :: forall e m a. (MonadCatch m, Exception e) => m a -> (e -> e) -> m a
mapExn task f = task `catch` (\ (e :: e) -> throw (f e))

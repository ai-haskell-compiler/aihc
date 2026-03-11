{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TemplateHaskellQuotes #-}
#endif
-- | This module provides TH helpers,
-- which use 'Solo' from this package, for 1-tuples.
module Data.Tuple.Solo.TH (
    tupE,
) where


#if MIN_VERSION_template_haskell(2,17,0)
import Language.Haskell.TH (tupE)
#else
import Data.Tuple.Solo

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

makeTup :: [TH.Exp] -> TH.Exp
makeTup [x] = TH.AppE (TH.ConE soloConName) x
#if MIN_VERSION_template_haskell(2,16,0)
makeTup xs  = TH.TupE (map Just xs)
#else
makeTup xs  = TH.TupE xs
#endif

soloConName :: TH.Name
#if __GLASGOW_HASKELL__ >= 800
soloConName = 'Solo
#else
#ifndef CURRENT_PACKAGE_KEY 
#error "CURRENT_PACKAGE_KEY undefined"
#endif

soloConName = TH.mkNameG_d CURRENT_PACKAGE_KEY "Data.Tuple.Solo" "MkSolo"
#endif

tupE :: Monad m => [m TH.Exp] -> m TH.Exp
tupE xs = do
    xs' <- sequence xs
    return (makeTup xs')
#endif


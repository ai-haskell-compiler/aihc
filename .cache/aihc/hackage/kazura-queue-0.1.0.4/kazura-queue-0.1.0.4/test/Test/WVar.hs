
module Test.WVar where

import qualified Test.Hspec      as HS
import qualified Test.QuickCheck as Q

import qualified Control.Concurrent.WVar as WV

import qualified Control.Monad as M

withLatestCache ::
    (IO (v, v, WV.WVar v, WV.WCached v) -> r) -> IO (v, WV.WVar v) -> r
withLatestCache f prepare = f prepare'
  where
    prepare' = do
        (val, wv) <- prepare
        wc        <- WV.cacheWVar wv
        return (val, val, wv, wc)

whenWVarIsFresh ::
    (IO (Int, WV.WVar Int) -> HS.Spec) -> HS.Spec
whenWVarIsFresh f = HS.describe "when WVar is fresh" $ f prepare
  where
    prepare = do
        val <- Q.generate Q.arbitrary
        wv  <- WV.newWVar val
        return (val, wv)

whenWVarIsUpdating :: Q.Arbitrary v =>
    (IO (v, WV.WVar v) -> HS.Spec) -> HS.Spec
whenWVarIsUpdating f = HS.describe "when WVar is updating" $ f prepare
  where
    prepare = do
        val <- Q.generate Q.arbitrary
        wv  <- WV.newWVar val
        M.void $ WV.takeWVar wv
        return (val, wv)

whenWVarIsFreshButCacheStaled :: (Eq v, Q.Arbitrary v) =>
    (IO (v, v, WV.WVar v, WV.WCached v) -> HS.Spec) -> HS.Spec
whenWVarIsFreshButCacheStaled f =
    HS.describe "when WVar is fresh but cache staled" $ f prepare
  where
    prepare = do
        val1 <- Q.generate Q.arbitrary
        val2 <- Q.generate $ Q.arbitrary `Q.suchThat` (/= val1)
        wv   <- WV.newWVar val1
        wc   <- WV.cacheWVar wv
        WV.putWVar wv val2
        return (val1, val2, wv, wc)

whenWVarIsUpdatingAndCacheStaled :: (Eq v, Q.Arbitrary v) =>
    (IO (v, v, WV.WVar v, WV.WCached v) -> HS.Spec) -> HS.Spec
whenWVarIsUpdatingAndCacheStaled f =
    HS.describe "when WVar is updating and cache staled" $ f prepare
  where
    prepare = do
        val <- Q.generate Q.arbitrary
        wv  <- WV.newWVar val
        wc  <- WV.cacheWVar wv
        M.void $ WV.takeWVar wv
        return (val, val, wv, wc)

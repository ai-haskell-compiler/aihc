-- |
-- Module: WildBind.X11.TestUtil
-- Description:
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
--
module WildBind.X11.TestUtil
    ( checkIfX11Available
    , withGrabs
    ) where

import           Control.Applicative ((<$>))
import           Control.Exception   (bracket)
import           Control.Monad       (when)
import           System.Environment  (lookupEnv)
import           Test.Hspec

import           WildBind            (FrontEnd (..))

isDisplaySet :: IO Bool
isDisplaySet = exists <$> lookupEnv "DISPLAY" where
  exists (Just v) = v /= ""
  exists _        = False

checkIfX11Available :: Spec -> Spec
checkIfX11Available = before $ do
  available <- isDisplaySet
  when (not available) $ pendingWith "DISPLAY env is not set. Skipped."

withGrabs :: FrontEnd s i -> [i] -> IO a -> IO a
withGrabs front inputs action = bracket grabAll (const ungrabAll) (const action)
  where
    grabAll = mapM_ (frontSetGrab front) inputs
    ungrabAll = mapM_ (frontUnsetGrab front) inputs


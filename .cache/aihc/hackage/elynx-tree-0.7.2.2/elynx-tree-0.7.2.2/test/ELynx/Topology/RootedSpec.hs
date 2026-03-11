-- |
-- Module      :  ELynx.Topology.RootedSpec
-- Description :  Unit tests for ELynx.Topology.Rooted
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Sat Jul 18 13:58:16 2020.
module ELynx.Topology.RootedSpec
  ( spec,
  )
where

import Data.Proxy
import ELynx.ClassLaws
import ELynx.Topology
import ELynx.Topology.Arbitrary ()
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Classes

type T = Topology Double

spec :: Spec
spec = do
  describe "Topology" $ do
    it "has reasonable applicative take right instance" $
      property (prop_appl_right :: T -> T -> Bool)
    it "has reasonable applicative take left instance" $
      property (prop_appl_left :: T -> T -> Bool)
    it "has reasonable applicative liftA2 instance" $
      property (prop_appl (*) :: T -> Bool)
    it "has reasonable applicative and functor instances" $
      property (prop_appl_func (+ 3) :: T -> Bool)
    lawsCheckSpec (functorLaws (Proxy :: Proxy Topology))
    lawsCheckSpec (foldableLaws (Proxy :: Proxy Topology))
    lawsCheckSpec (traversableLaws (Proxy :: Proxy Topology))
    lawsCheckSpec (applicativeLaws (Proxy :: Proxy Topology))
    lawsCheckSpec (monadLaws (Proxy :: Proxy Topology))

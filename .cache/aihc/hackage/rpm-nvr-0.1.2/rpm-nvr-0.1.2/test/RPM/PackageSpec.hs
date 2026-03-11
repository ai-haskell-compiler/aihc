module RPM.PackageSpec (pkgspec) where

import           Test.Hspec
import           Data.RPM.NV
import           Data.RPM.NVR
import           Data.RPM.NVRA

import Data.Maybe (isNothing)
import System.FilePath (dropExtension, takeFileName)

-- FIXME add some failures too
pkgspec :: Spec
pkgspec = do
  describe "NV" $ do
    let nv = "my-pkg-1.0.1" in
      it nv $
      showNV (readNV nv) `shouldBe` nv

    let v = "1.0.1" in
      it v $
      isNothing (maybeNV v) `shouldBe` True

  describe "NVR" $ do
    let nvr = "my-pkg-1.0.1-1.2" in
      it nvr $
      showNVR (readNVR nvr) `shouldBe` nvr

    let nv = "name-1.0.1" in
      it nv $
      isNothing (maybeNVR nv) `shouldBe` True

  describe "NVRA" $ do
    let pkg = "my-pkg-1.0.1-1.2.x86_64" in
      it pkg $
      showNVRA (readNVRA pkg) `shouldBe` pkg

    let nv = "name-1.0.1" in
      it nv $
      isNothing (maybeNVRA nv) `shouldBe` True

  describe "NVRA.rpm" $ do
    let pkg = "my-pkg-1.0.1-1.2.x86_64.rpm" in
      it pkg $
      showNVRA (readNVRA pkg) `shouldBe` dropExtension pkg

  describe "dir/NVRA.rpm" $ do
    let pkg = "dir/my-pkg-1.0.1-1.2.x86_64.rpm" in
      it pkg $
      showNVRA (readNVRA pkg) `shouldBe` dropExtension (takeFileName pkg)

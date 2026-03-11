import Test.Hspec
import Control.Monad.Extra
import Data.Version.Extra
--import Distribution.RPM.Build.Graph
import Distribution.RPM.Build.Order
import SimpleCmd
import System.Directory (withCurrentDirectory)
import System.Posix.Files

main :: IO ()
main = do
  setupSymlinks
  -- rpmspec < 4.15.1 does not support "--with"
  rpmver <- readVersion . last . words <$> cmd "rpmspec" ["--version"]
  hspec (spec rpmver)

pkg :: FilePath -> FilePath
pkg = ("test/pkgs/" ++)

-- FIXME refactor to test both subdir and withCurrentDirectory
spec :: Version -> Spec
spec rpmver = do
  describe "sort" $ do
    it "A B" $
      both dependencySort [pkg "A", pkg "B"] >>=
      (`shouldBe` [pkg "B", pkg "A"])

    it "1 2" $
      both dependencySort [pkg "1", pkg "2"] >>=
      (`shouldBe` [pkg "2", pkg "1"])

    it "A/ B/" $
      both dependencySort [pkg "A/", pkg "B/"] >>=
      (`shouldBe` [pkg "B/", pkg "A/"])

    it "1/ 2/" $
      both dependencySort [pkg "1/", pkg "2/"] >>=
      (`shouldBe` [pkg "2/", pkg "1/"])

    it "A.spec B.spec" $
      both dependencySort [pkg "A/A.spec", pkg "B/B.spec"] >>=
      (`shouldBe` [pkg "B/B.spec", pkg "A/A.spec"])

    it "1.spec 2.spec" $
      both dependencySort [pkg "1/A.spec", pkg "2/B.spec"] >>=
      (`shouldBe` [pkg "2/B.spec", pkg "1/A.spec"])

    it "A/ B.spec" $
      both dependencySort [pkg "A/", pkg "B/B.spec"] >>=
      (`shouldBe` [pkg "B/B.spec", pkg "A/"])

    it "circular A B C" $
      both dependencySort [pkg "A", pkg "B", pkg "C"]
      `shouldThrow` anyException

    it "A B D1.0" $
      dependencySort [pkg "A", pkg "B/", pkg "D1.0"] >>=
      (`shouldBe` [pkg "B/", pkg "D1.0", pkg "A"])

    it "A D" $
      indy dependencySort [pkg "A", pkg "D1.0"] >>=
      (`shouldBe` [pkg "A", pkg "D1.0"])

    it "C D" $
      indy dependencySort [pkg "C", pkg "D1.0"] >>=
      (`shouldBe` [pkg "C", pkg "D1.0"])

    it "pkgconf" $
      both dependencySort [pkg "pkgconf/C", pkg "pkgconf/A"] >>=
      (`shouldBe` [pkg "pkgconf/C", pkg "pkgconf/A"])

    it "cmake" $
      both dependencySort [pkg "cmake/A", pkg "cmake/B"] >>=
      (`shouldBe` [pkg "cmake/B", pkg "cmake/A"])

    when (rpmver > makeVersion [4,15,0]) $
      it "dynbr A B" $
      both dependencySort [pkg "dynbr/A", pkg "B"] >>=
      (`shouldBe` [pkg "B", pkg "dynbr/A"])

    when (rpmver > makeVersion [4,15,1]) $
      it "circular A B C boot" $
      both (dependencySortRpmOpts ["--with=boot"]) [pkg "A", pkg "B", pkg "C"] >>=
      (`shouldBe` [pkg "C", pkg "B", pkg "A"])

  describe "layers" $
    it "A B" $
      dependencyLayers [pkg "A", pkg "B"] >>=
      (`shouldBe` [[pkg "B"], [pkg "A"]])

  describe "leaves" $
    it "A B D" $
      leafPackages [pkg "A", pkg "B", pkg "D1.0"] >>=
      (`shouldBe` [pkg "A", pkg "D1.0"])

  let rpmbuildOrder f c = fmap f . cmd "rpmbuild-order" . (c:)

  describe "rpmbuild-order" $ do
    it "sort A B" $
      both (rpmbuildOrder words "sort") [pkg "A", pkg "B"] >>=
      (`shouldBe` [pkg "B", pkg "A"])

    it "sort A D" $
      indy (rpmbuildOrder lines "sort") [pkg "A", pkg "D1.0"] >>=
      (`shouldBe` [pkg "D1.0", "", pkg "A"])

    it "sort pkgconf" $
      both (rpmbuildOrder words "sort") [pkg "pkgconf/A", pkg "pkgconf/C"] >>=
      (`shouldBe` [pkg "pkgconf/C", pkg "pkgconf/A"])

    it "sort cmake" $
      both (rpmbuildOrder words "sort") [pkg "cmake/A", pkg "cmake/B"] >>=
      (`shouldBe` [pkg "cmake/B", pkg "cmake/A"])

    it "deps A" $
      withCurrentDirectory "test/pkgs" $
      cmd "rpmbuild-order"
      ["deps", "-x", "dynbr", "-x", "pkgconf", "-x", "1", "-x", "2", "-x", "C","-x", "cmake", "A"] >>=
      (`shouldBe` unwords ["B", "A"])
  where
    both :: ([FilePath] -> IO [FilePath]) -> [FilePath] -> IO [FilePath]
    both io fs = do
      r <- io fs
      r' <- io $ reverse fs
      r `shouldBe` r'
      return r

    indy :: ([FilePath] -> IO [FilePath]) -> [FilePath] -> IO [FilePath]
    indy io fs = do
      r <- io fs
      r' <- io $ reverse fs
      r `shouldBe` reverse r'
      return r'

setupSymlinks :: IO ()
setupSymlinks =
  forM_ [("1","A"),("2","B")] $ \ (l,f) ->
    unlessM (fileExist $ pkg l) $
    createSymbolicLink f (pkg l)

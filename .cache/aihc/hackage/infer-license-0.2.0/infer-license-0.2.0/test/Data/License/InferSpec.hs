{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Data.License.InferSpec (spec) where
import           Test.Hspec

import           Control.Monad
import           System.Directory
import           System.FilePath

import           Data.License.Infer

readLicenseFiles :: FilePath -> IO [(FilePath, String)]
readLicenseFiles file = do
  doesDirectoryExist file >>= \ case
    True -> do
      files <- listDirectory file
      forM files $ \ ((file </>) -> p) -> do
        (,) p <$> readFile p
    False -> return . (,) file <$> readFile file

spec :: Spec
spec = do
  describe "inferLicense" $ do
    forM_ [minBound .. maxBound] $ \ l -> do
      let license = show l

      licenses <- runIO $ readLicenseFiles ("test/resources/" </> license)
      forM_ licenses $ \ (file, content) -> do
        context ("with " ++ file) $ do
          it ("infers " ++ license) $ do
            inferLicense content `shouldBe` Just l

    it "rejects unknown licenses" $ do
      inferLicense (unwords $ replicate 10000 "unknown") `shouldBe` Nothing

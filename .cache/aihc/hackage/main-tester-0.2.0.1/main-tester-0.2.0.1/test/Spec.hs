import           Control.Monad (mapM_)
import           Data.ByteString.Char8 (pack)
import qualified Control.Exception as E
import           System.Environment (getArgs, setEnv, lookupEnv)
import           System.Exit (ExitCode(ExitSuccess, ExitFailure) , exitWith)
import           System.IO (stderr, hPutStr)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
                   ( Arbitrary
                   , arbitrary
                   , choose
                   , oneof
                   , listOf
                   )

import           Test.Main
import           Test.Main.Internal


newtype AExitCode = AExitCode ExitCode deriving Show

instance Arbitrary AExitCode where
  arbitrary =
    AExitCode <$> oneof [pure ExitSuccess, ExitFailure <$> choose (1, maxBound)]


newtype PrintableAsciiString =
  PrintableAsciiString { getPrintableAsciiString :: String } deriving Show

-- https://en.wikipedia.org/wiki/ASCII#Character_set
instance Arbitrary PrintableAsciiString where
  arbitrary = PrintableAsciiString <$> listOf (choose (' ', '~'))


main :: IO ()
main = hspec $
  describe "Test.Main" $ do
    prop "passes stdin and arguments to the program, and captures stdin data, stderr data, and exit code" $
      \(argsA, inDataA, AExitCode eCode) -> do
        let args = map getPrintableAsciiString argsA
            inData = pack $ getPrintableAsciiString inDataA
            testedMain = do
              mapM_ putStrLn =<< getArgs
              hPutStr stderr =<< getContents
              exitWith eCode

        actual <- withStdin inData $ withArgs args $ captureProcessResult testedMain

        normalizeNewLines actual `shouldBe`
          normalizeNewLines (ProcessResult (pack $ unlines args) inData eCode Nothing)

    prop "captures stdin data, stderr data, exit code, and thrown exception when the program throws an exception" $
      \(inDataA, errDataA, errorMesage) -> do
        let inData = pack $ getPrintableAsciiString inDataA
            errDataS = getPrintableAsciiString errDataA
            errData = pack errDataS
            testedMain = do
              putStr =<< getContents
              hPutStr stderr errDataS
              _ <- fail errorMesage
              putStrLn "*** THIS SHOULD NOT BE PRINTED! ***"

        actual <- withStdin inData $ captureProcessResult testedMain

        normalizeNewLines actual `shouldBe`
          normalizeNewLines
            ( ProcessResult
                inData
                errData
                (ExitFailure 1)
                (Just $ E.SomeException $ userError errorMesage)
            )

    it "overwrites or deletes the specified environment variables temporarily" $ do
      let vu = "value_to_unset"
          vw = "value_to_overwrite"
          nv = "new_value"

      setEnv "ENV_VAR_TO_UNSET"     vu
      setEnv "ENV_VAR_TO_OVERWRITE" vw

      let action =
            (,) <$> lookupEnv "ENV_VAR_TO_UNSET" <*> lookupEnv "ENV_VAR_TO_OVERWRITE"
      resultMutated <- withEnv
        [("ENV_VAR_TO_UNSET", Nothing), ("ENV_VAR_TO_OVERWRITE", Just nv)]
        action

      resultMutated `shouldBe` (Nothing, Just nv)
      action `shouldReturn` (Just vu, Just vw)

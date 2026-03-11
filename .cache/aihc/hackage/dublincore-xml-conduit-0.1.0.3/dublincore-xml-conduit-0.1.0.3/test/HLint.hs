import qualified Language.Haskell.HLint as HLint (hlint)
import System.Exit

main :: IO ()
main = do
  result <- HLint.hlint ["test/", "src/"]
  if null result then exitSuccess else exitFailure

import Aihc.Lir.Resolve (loadModule, renderLoadError)
import Aihc.Llvm.Lir (compileLirModule)
import qualified Data.Text.IO as T
import System.Environment (getArgs)

main :: IO ()
main = do
  [source, output] <- getArgs
  lir <- loadModule source >>= either (fail . renderLoadError) pure
  llvm <- either (fail . show) pure (compileLirModule lir)
  T.writeFile output llvm

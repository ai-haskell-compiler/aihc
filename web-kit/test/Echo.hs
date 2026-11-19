import Data.Char (toUpper)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  input <- getContents
  putStr (map toUpper input)
  hPutStrLn stderr (show (length (lines input)) ++ " lines")
  exitFailure

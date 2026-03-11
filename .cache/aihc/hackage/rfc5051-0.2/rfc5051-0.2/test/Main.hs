import System.Exit
import qualified Data.Text as T
import Data.RFC5051 (compareUnicode)
import Data.List (sortBy)

main :: IO ()
main = do
  let ls = map T.pack ["Abe", "Oeb", "abe", "ab\233", "oeb",
                       "\193be", "\196be", "\212eb", "\225be",
                       "\228be", "\244eb"]
  let expected = map T.pack
                      ["Abe", "abe", "ab\233", "\193be", "\225be",
                       "\196be", "\228be", "Oeb", "oeb", "\212eb",
                       "\244eb"]
  let actual = sortBy compareUnicode ls
  if actual == expected
     then exitWith ExitSuccess
     else do
       putStrLn "FAILED"
       putStrLn $ "EXPECTED: " ++ show expected
       putStrLn $ "GOT:      " ++ show actual
       exitWith $ ExitFailure 1

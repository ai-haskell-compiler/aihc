import Data.URI.Auth.Host (printURIAuthHost, parseURIAuthHost)
import Data.URI.Auth (printURIAuth, parseURIAuth)
import Data.URI (printURI, parseURI)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text (Parser, parse, IResult (..), endOfInput)
import Test.Tasty (testGroup, defaultMain)
import qualified Test.Tasty.QuickCheck as Q
import Test.QuickCheck.Property (Result, succeeded, failed)
import System.IO.Unsafe (unsafePerformIO)



main :: IO ()
main = defaultMain $ testGroup "URI tests"
  [ Q.testProperty "URIAuthHost" (parsePrintIso printURIAuthHost parseURIAuthHost)
  , Q.testProperty "URIAuth" (parsePrintIso printURIAuth parseURIAuth)
  , Q.testProperty "URI" (parsePrintIso printURI parseURI)
  ]


parsePrintIso :: Eq a => Show a => (a -> Text) -> Parser a -> a -> Result
parsePrintIso print' parser x =
  let value = parse (parser <* endOfInput) (print' x)
      run q = case q of
        Fail leftover es e ->
          let go = unsafePerformIO $ do
                putStr "\nOriginal value: "
                print x
                putStr "Printed Text: "
                putStrLn $ T.unpack $ print' x
                putStr "Parse Error: "
                print e
                putStr "Parse Error Stack: "
                print es
                putStr "Leftover text: "
                putStrLn $ T.unpack leftover
          in  seq go failed
        Partial f -> run (f T.empty)
        Done leftover y
          | y == x -> succeeded
          | otherwise ->
            let go = unsafePerformIO $ do
                  putStr "\nOriginal value: "
                  print x
                  putStr "Printed Text: "
                  putStrLn $ T.unpack $ print' x
                  putStr "Parsed Value: "
                  print y
                  putStr "Leftover text: "
                  putStrLn $ T.unpack leftover
            in  seq go failed
  in  run value

# fakepull

Monad to pull from fake stream-like objects.

## Example

Sometimes you might want to test a function that uses an HTTP client object to
get responses from some web server, without actually making the HTTP requests.

```haskell
-- BEGINNING OF EXAMPLE

import Control.Monad.IO.Class
import Data.IORef
import Data.Maybe
import Test.Hspec
import Test.Pull.Fake.IO


data Request = Request
  { searchTerm :: String
  , pageSize :: Int
  , cursor :: Maybe String
  } deriving (Eq, Show)


data Response = Response
  { searchResult :: [String]
  , nextCursor :: Maybe String
  } deriving (Eq, Show)


-- | The function you test.
--   Call `sendRequest` repeatedly to get all paginated search results using
--   the cursor.
fetchAllPages :: MonadIO m => (Request -> m Response) -> String -> m [String]
fetchAllPages sendRequest term = go [] (Request term 3 Nothing)
 where
  go accum req = do
    res <- sendRequest req
    let newAccum = accum ++ searchResult res
    case nextCursor res of
        Just cur -> do
          let newReq = req { cursor = Just cur }
          go newAccum newReq
        Nothing -> return newAccum


-- | The function to make an HTTP request actually. Will be stubbed.
sendRequestActually :: Request -> IO Response
sendRequestActually = error "This function should be stubbed!"

-- | Simulate the `sendRequest` function that returns a different result every
--   time.
stubbedSendRequest :: FakeStream Response -> Request -> IO Response
stubbedSendRequest stream _request =
  -- NOTE: You may want to validate the request argument for testing.
  --       I've omitted that for simplicity here.
  fromJust <$> pull stream


main :: IO ()
main = hspec $
  describe "fetchAllPages" $
    it "collect all results by sending requests" $ do
      let allResponses =
            [ Response ["result 1-1", "result 1-2", "result 1-3"] $ Just "cursor a"
            , Response ["result 2-2", "result 2-2", "result 2-3"] $ Just "cursor b"
            , Response ["result 3-1"] Nothing
            ]

      responsesToReturn <- newFakeStream allResponses
      fetchAllPages (stubbedSendRequest responsesToReturn) "result" 
        `shouldReturn` concatMap searchResult allResponses

-- END
```

## Related package

- [fakefs](http://hackage.haskell.org/package/fakefs)
    - This package is a variant of fakefs for stream-like objects.

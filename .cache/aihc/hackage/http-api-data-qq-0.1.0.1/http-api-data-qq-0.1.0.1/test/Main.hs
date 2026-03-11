import Test.Tasty

import qualified Web.HttpApiData.QQ.ParserTest
import qualified Web.HttpApiData.QQTest

main :: IO ()
main =
  defaultMain . testGroup "http-api-data-qq" $
    [ Web.HttpApiData.QQ.ParserTest.tests
    , Web.HttpApiData.QQTest.tests
    ]

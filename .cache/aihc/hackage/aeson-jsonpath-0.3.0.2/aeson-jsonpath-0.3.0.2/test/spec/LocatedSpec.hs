module LocatedSpec
  ( spec )
  where

import qualified Data.Aeson        as JSON
import qualified Data.Vector       as V

import Data.Aeson.JSONPath  (queryLocatedQQ, jsonPath)
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Aeson           (Value)
import Data.Vector          (Vector)
import Test.Hspec

import Prelude

getVector :: Value -> Vector Value
getVector (JSON.Array arr) = arr
getVector _ = V.empty

-- taken from https://serdejsonpath.live/
rootDoc :: Value
rootDoc = [aesonQQ|{
    "store": {
      "books": [
        {
          "title": "Guns, Germs, and Steel",
          "author": "Jared Diamond",
          "category": "reference",
          "price": 24.99
        },
        {
          "title": "David Copperfield",
          "author": "Charles Dickens",
          "category": "fiction",
          "price": 12.99
        },
        {
          "title": "Moby Dick",
          "author": "Herman Melville",
          "category": "fiction",
          "price": 8.99
        },
        {
          "title": "Crime and Punishment",
          "author": "Fyodor Dostoevsky",
          "category": "fiction",
          "price": 19.99
        }
      ]
    }
  }|]


storeDoc :: Value
storeDoc = [aesonQQ|
  {
    "books": [
      {
        "title": "Guns, Germs, and Steel",
        "author": "Jared Diamond",
        "category": "reference",
        "price": 24.99
      },
      {
        "title": "David Copperfield",
        "author": "Charles Dickens",
        "category": "fiction",
        "price": 12.99
      },
      {
        "title": "Moby Dick",
        "author": "Herman Melville",
        "category": "fiction",
        "price": 8.99
      },
      {
        "title": "Crime and Punishment",
        "author": "Fyodor Dostoevsky",
        "category": "fiction",
        "price": 19.99
      }
    ]
  }|]

booksDoc :: Value
booksDoc = [aesonQQ| [
      {
        "title": "Guns, Germs, and Steel",
        "author": "Jared Diamond",
        "category": "reference",
        "price": 24.99
      },
      {
        "title": "David Copperfield",
        "author": "Charles Dickens",
        "category": "fiction",
        "price": 12.99
      },
      {
        "title": "Moby Dick",
        "author": "Herman Melville",
        "category": "fiction",
        "price": 8.99
      },
      {
        "title": "Crime and Punishment",
        "author": "Fyodor Dostoevsky",
        "category": "fiction",
        "price": 19.99
      }
  ]|]


books0Doc :: Value
books0Doc = [aesonQQ|[
      {
        "title": "Guns, Germs, and Steel",
        "author": "Jared Diamond",
        "category": "reference",
        "price": 24.99
      }
  ]|]


spec :: Spec
spec = do
  describe "test queryLocated" $ do
    it "returns root document when query is $" $
      queryLocatedQQ [jsonPath|$|] rootDoc `shouldBe` V.singleton ("$",rootDoc)

    it "returns store object when queryQQ is $.store" $
      queryLocatedQQ [jsonPath|$.store|] rootDoc `shouldBe` V.fromList [("$['store']",storeDoc)]

    it "returns store object when queryQQ is $['store']" $
      queryLocatedQQ [jsonPath|$['store']|] rootDoc `shouldBe` V.fromList [("$['store']",storeDoc)]

    it "returns books array when queryQQ is $.store.books" $
      queryLocatedQQ [jsonPath|$.store.books|] rootDoc `shouldBe` V.fromList [("$['store']['books']",booksDoc)]

    it "returns 0-index book item, $.store.books[0]" $
      queryLocatedQQ [jsonPath|$.store.books[0]|] rootDoc `shouldBe` V.zip (V.singleton "$['store']['books'][0]") (getVector books0Doc)

    it "returns 0-index book item, $.store.books[-4]" $
      queryLocatedQQ [jsonPath|$.store.books[-4]|] rootDoc `shouldBe` V.zip (V.singleton "$['store']['books'][0]") (getVector books0Doc)

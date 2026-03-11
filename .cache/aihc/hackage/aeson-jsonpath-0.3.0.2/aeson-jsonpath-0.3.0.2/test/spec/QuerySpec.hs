module QuerySpec
  ( spec )
  where

import qualified Data.Vector       as V
import qualified Data.Aeson        as JSON

import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Aeson           (Value)
import Data.Vector          (Vector)
import Test.Hspec

import Data.Aeson.JSONPath  (queryQQ, jsonPath)

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

books0And2Doc :: Value
books0And2Doc = [aesonQQ|[
      {
        "title": "Guns, Germs, and Steel",
        "author": "Jared Diamond",
        "category": "reference",
        "price": 24.99
      },
      {
        "title": "Moby Dick",
        "author": "Herman Melville",
        "category": "fiction",
        "price": 8.99
      }
  ]|]

books1To3Doc :: Value
books1To3Doc = [aesonQQ|[
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
      }
  ]|]

books1To3And0And1Doc :: Value
books1To3And0And1Doc = [aesonQQ|[
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
      }
  ]|]

lessThanPrice20Books :: Value
lessThanPrice20Books = [aesonQQ| [
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

alphaArr :: Value
alphaArr = [aesonQQ| ["a","b","c","d","e","f","g"] |]

fgArr :: Value
fgArr = [aesonQQ| ["f","g"] |]

bdArr :: Value
bdArr = [aesonQQ| ["b","d"] |]

fdArr :: Value
fdArr = [aesonQQ| ["f","d"] |]

gfedcbaArr :: Value
gfedcbaArr = [aesonQQ| ["g","f","e","d","c","b","a"] |]

rfcExample1 :: Value
rfcExample1 = [aesonQQ| {
    "o": {"j": 1, "k": 2},
    "a": [5, 3, [{"j": 4}, {"k": 6}]]
  } |]

rfcExample1Desc :: Value
rfcExample1Desc = [aesonQQ| [
    [5, 3, [{"j": 4}, {"k": 6}]],
    {"j": 1, "k": 2},
    5,
    3,
    [{"j": 4}, {"k": 6}],
    {"j": 4},
    {"k": 6},
    4,
    6,
    1,
    2
  ]|]

boolsAndNulls :: Value
boolsAndNulls = [aesonQQ| [
    { "a": true },
    { "a": false },
    { "a": null }
  ]|]


boolsAndNullsA :: Value
boolsAndNullsA = [aesonQQ| [{ "a": true }] |]

boolsAndNullsB :: Value
boolsAndNullsB = [aesonQQ| [{ "a": false }] |]

boolsAndNullsC :: Value
boolsAndNullsC = [aesonQQ| [{ "a": null }] |]

spec :: Spec
spec = do
  describe "test query" $ do
    it "returns root document when query is $" $
      queryQQ [jsonPath|$|] rootDoc `shouldBe` V.singleton rootDoc

    it "returns store object when queryQQ is $.store" $
      queryQQ [jsonPath|$.store|] rootDoc `shouldBe` V.singleton storeDoc

    it "returns store object when queryQQ is $['store']" $
      queryQQ [jsonPath|$['store']|] rootDoc `shouldBe` V.singleton storeDoc

    it "returns books array when queryQQ is $.store.books" $
      queryQQ [jsonPath|$.store.books|] rootDoc `shouldBe` V.singleton booksDoc

    it "returns 0-index book item when queryQQ is $.store.books[0]" $
      queryQQ [jsonPath|$.store.books[0]|] rootDoc `shouldBe` getVector books0Doc

    it "returns 0-index book item when queryQQ is $.store.books[-4]" $
      queryQQ [jsonPath|$.store.books[-4]|] rootDoc `shouldBe` getVector books0Doc

    it "returns 0,2-index item when queryQQ is $.store.books[0,2]" $
      queryQQ [jsonPath|$.store.books[0,2]|] rootDoc `shouldBe` getVector books0And2Doc

    it "returns 1To3-index when queryQQ is $.store.books[1:3]" $
      queryQQ [jsonPath|$.store.books[1:3]|] rootDoc `shouldBe` getVector books1To3Doc

    it "returns 1To3-index and 0,1-index when queryQQ is $.store.books[1:3,0,1]" $
      queryQQ [jsonPath|$.store.books[1:3,0,1]|] rootDoc `shouldBe` getVector books1To3And0And1Doc

    it "returns slice with queryQQ $[5:]" $
      queryQQ [jsonPath|$[5:]|] alphaArr `shouldBe` getVector fgArr

    it "returns slice with queryQQ $[1:5:2]" $
      queryQQ [jsonPath|$[1:5:2]|] alphaArr `shouldBe` getVector bdArr

    it "returns slice with queryQQ $[5:1:-2]" $
      queryQQ [jsonPath|$[5:1:-2]|] alphaArr `shouldBe` getVector fdArr

    it "returns slice with queryQQ $[::-1]" $
      queryQQ [jsonPath|$[::-1]|] alphaArr `shouldBe` getVector gfedcbaArr

    it "returns root with queryQQ $.*" $
      queryQQ [jsonPath|$.*|] rootDoc `shouldBe` V.singleton storeDoc

    it "returns root with queryQQ $[*]" $
      queryQQ [jsonPath|$[*]|] rootDoc `shouldBe` V.singleton storeDoc

    it "returns descendants with queryQQ $..*" $
      queryQQ [jsonPath|$..*|] rfcExample1 `shouldBe` getVector rfcExample1Desc

    it "returns descendants with queryQQ $..[*]" $
      queryQQ [jsonPath|$..[*]|] rfcExample1 `shouldBe` getVector rfcExample1Desc

    it "returns with filtering: number comparison" $
      queryQQ [jsonPath|$.store.books[?@.price < 20]|] rootDoc
      `shouldBe` getVector lessThanPrice20Books

    it "returns with filtering: not operator" $
      queryQQ [jsonPath|$.store.books[?!(@.price < 20)]|] rootDoc
      `shouldBe` getVector books0Doc

    it "returns with filtering: true value" $
      queryQQ [jsonPath|$[?@.a == true]|] boolsAndNulls
      `shouldBe` getVector boolsAndNullsA

    it "returns with filtering: false value" $
      queryQQ [jsonPath|$[?@.a == false]|] boolsAndNulls
      `shouldBe` getVector boolsAndNullsB

    it "returns with filtering: null value" $
      queryQQ [jsonPath|$[?@.a == null]|] boolsAndNulls
      `shouldBe` getVector boolsAndNullsC

    it "returns with filtering: string comparison" $
      queryQQ [jsonPath|$.store.books[?@.author == 'Jared Diamond']|] rootDoc
      `shouldBe` getVector books0Doc

    it "returns with filtering: test expression" $
      queryQQ [jsonPath|$.store.books[?@.author]|] rootDoc
      `shouldBe` getVector booksDoc

    it "returns with filtering with spaced out segments: test expression" $
      queryQQ [jsonPath|$  .store  .books[?@  .author]|] rootDoc
      `shouldBe` getVector booksDoc

    it "returns with filtering: test expr gives empty with non-existent key" $
      queryQQ [jsonPath|$.store.books[?@.not_here]|] rootDoc
      `shouldBe` V.empty

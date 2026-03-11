module Network.Control.LRUCacheSpec (spec) where

import Data.Maybe
import Network.Control
import qualified Network.Control as LRU
import Test.Hspec

spec :: Spec
spec = do
    describe "LRUCache" $ do
        it "can keep entry if looked up" $ do
            let cache = insert 'b' "bar" $ insert 'a' "foo" $ empty 2
                (v, cache') = fromJust $ LRU.lookup' 'a' cache
            v `shouldBe` "foo"
            let cache'' = insert 'c' "baz" cache'
            fst <$> LRU.lookup' 'a' cache'' `shouldBe` Just "foo"
            fst <$> LRU.lookup' 'b' cache'' `shouldBe` Nothing
            fst <$> LRU.lookup' 'c' cache'' `shouldBe` Just "baz"
        it "can rebuild PSQ when reached the limit" $ do
            let cache = insert 'b' "bar" $ insert 'a' "foo" $ empty' 2 (maxBound - 2)
            show cache
                `shouldBe` "LRUCache {lcLimit = 2, lcTick = 2, lcQueue = Winner (E 'a' 0 \"foo\") (RLoser 1 (E 'b' 1 \"bar\") Start 'a' Start) 'b'}"

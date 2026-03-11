import Data.Maybe
import Data.Text (pack)
import Test.Hspec
import Network.HTTP.Directory

main :: IO ()
main = hspec $ parallel spec

spec :: Spec
spec = do
  describe "httpDirectory" $ do
    it "empty httpbin" $ do
      mgr <- httpManager
      fs <- httpDirectory mgr "http://httpbin.org/"
      fs `shouldBe` []

    it "fedora releases" $ do
      fs <- httpDirectory' "https://dl.fedoraproject.org/pub/fedora/linux/releases/"
      null fs `shouldBe` False

    it "my src hackage" $ do
      fs <- httpDirectory' "https://hackage.haskell.org/package/http-directory/src/"
      null fs `shouldBe` False

    it "self hackage" $ do
      fs <- httpDirectory' "https://hackage.haskell.org/package/http-directory"
      null fs `shouldBe` False

    it "self hackage" $ do
      fs <- httpDirectory' "https://mirrors.kernel.org/"
      null fs `shouldBe` False

    it "404" $
      httpDirectory' "https://httpbin.org/404"
      `shouldThrow` anyException

  describe "httpExists" $ do
    it "google" $ do
      mgr <- httpManager
      exists <- httpExists mgr "https://google.com/"
      exists `shouldBe` True

    it "404" $ do
      mgr <- httpManager
      exists <- httpExists mgr "https://httpbin.org/404"
      exists `shouldBe` False

    it "domain" $ do
      mgr <- httpManager
      httpExists mgr "http://nowhereparticular"
      `shouldThrow` anyException

  describe "httpExists'" $ do
    it "google" $ do
      exists <- httpExists' "https://google.com/"
      exists `shouldBe` True

    it "404" $ do
      exists <- httpExists' "https://httpbin.org/404"
      exists `shouldBe` False

    it "domain" $ do
      httpExists' "http://nowhereparticular"
      `shouldThrow` anyException

  describe "httpFileSize" $ do
    it "httpbin/get" $ do
      mgr <- httpManager
      msize <- httpFileSize mgr "https://httpbin.org/get"
      isJust msize `shouldBe` True

    it "httpbin/0B" $ do
      mgr <- httpManager
      msize <- httpFileSize mgr "https://httpbin.org/bytes/0"
      msize `shouldBe` Just 0

    it "httpbin/64B" $ do
      mgr <- httpManager
      msize <- httpFileSize mgr "https://httpbin.org/bytes/64"
      msize `shouldBe` Just 64

    it "cabal" $ do
      mgr <- httpManager
      msize <- httpFileSize mgr "https://raw.githubusercontent.com/juhp/http-directory/master/http-directory.cabal"
      isJust msize `shouldBe` True

  describe "httpFileSize'" $ do
    it "httpbin/get" $ do
      msize <- httpFileSize' "https://httpbin.org/get"
      isJust msize `shouldBe` True

    it "httpbin/0B" $ do
      msize <- httpFileSize' "https://httpbin.org/bytes/0"
      msize `shouldBe` Just 0

    it "httpbin/64B" $ do
      msize <- httpFileSize' "https://httpbin.org/bytes/64"
      msize `shouldBe` Just 64

    it "cabal" $ do
      msize <- httpFileSize' "https://raw.githubusercontent.com/juhp/http-directory/master/http-directory.cabal"
      isJust msize `shouldBe` True

  describe "httpLastModified" $ do
    it "haskell" $ do
      mgr <- httpManager
      mtime <- httpLastModified mgr "https://www.haskell.org/"
      isJust mtime `shouldBe` True

    it "haskell" $ do
      mtime <- httpLastModified' "https://www.haskell.org/"
      isJust mtime `shouldBe` True

  describe "httpFileSizeTime" $ do
    it "haskell" $ do
      (msize, mtime) <- httpFileSizeTime' "https://haskell.org/"
      (isJust msize, isJust mtime) `shouldBe` (False,True)

    it "hf" $ do
      (msize, mtime) <- httpFileSizeTime' "https://haskell.foundation/"
      (isJust msize, isJust mtime) `shouldBe` (True,True)

  -- https://github.com/postmanlabs/httpbin/issues/617
  describe "httpRedirect" $ do
    it "httpRedirect" $ do
      mgr <- httpManager
      mredir <- httpRedirect mgr "http://httpbin.org/redirect/1"
      isJust mredir `shouldBe` True

    it "httpRedirect'" $ do
      mredir <- httpRedirect' "http://httpbin.org/redirect/1"
      isJust mredir `shouldBe` True

    it "2 httpRedirects" $ do
      mgr <- httpManager
      redirs <- httpRedirects mgr "http://httpbin.org/redirect/2"
      length redirs `shouldBe` 2

  describe "isHttpUrl" $ do
    it "http" $
      all isHttpUrl
        ["http://dl.fedoraproject.org", "https://haskell.org/"]
          `shouldBe` True

    it "non-http" $
      any isHttpUrl
         ["mailto:one@where", "somefile", "an.iso", "package.tgz"]
          `shouldBe` False

  describe "trailingSlash" $ do
    it "add" $
      trailingSlash "http://example.com/dir" `shouldBe` "http://example.com/dir/"

    it "idempotent" $
      trailingSlash "http://example.com/" `shouldBe` "http://example.com/"

    it "remove" $
      noTrailingSlash (pack "abc/") `shouldBe` pack "abc"

    it "remove all" $
      noTrailingSlash (pack "abc/def//") `shouldBe` pack "abc/def"

  describe "+/+" $ do
    it "join" $
      "http://example.com/dir" +/+ "file" `shouldBe` "http://example.com/dir/file"

    it "join/" $
      "http://example.com/dir" +/+ "/file" `shouldBe` "http://example.com/dir/file"

    it "/join" $
      "http://example.com/dir/" +/+ "file" `shouldBe` "http://example.com/dir/file"

    it "/join/" $
      "http://example.com/dir/" +/+ "/file" `shouldBe` "http://example.com/dir/file"

    it "join /" $
      "http://example.com/dir" +/+ "file/" `shouldBe` "http://example.com/dir/file/"

    it "identity" $
      "http://example.com/dir" +/+ "" `shouldBe` "http://example.com/dir"

    it "/identity" $
      "http://example.com/dir/" +/+ "" `shouldBe` "http://example.com/dir/"

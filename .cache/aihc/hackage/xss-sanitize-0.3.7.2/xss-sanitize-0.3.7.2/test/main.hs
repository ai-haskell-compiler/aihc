{-# LANGUAGE OverloadedStrings #-}
import Text.HTML.SanitizeXSS
import Text.HTML.SanitizeXSS.Css
import Data.Text (Text)

import Test.Hspec
import Test.HUnit (assert, (@?=), Assertion)

test :: (Text -> Text) -> Text -> Text -> Assertion
test f actual expected = do
  let result = f actual
  result @?= expected

sanitized, sanitizedB, sanitizedC :: Text -> Text -> Expectation
sanitized = test sanitize
sanitizedB = test sanitizeBalance
sanitizedC = test sanitizeCustom

sanitizeCustom :: Text -> Text
sanitizeCustom = filterTags $ safeTagsCustom mySafeName mySanitizeAttr
  where
    mySafeName t = t `elem` myTags || safeTagName t
    mySanitizeAttr (key, val) | key `elem` myAttrs = Just (key, val)
    mySanitizeAttr x = sanitizeAttribute x
    myTags = ["custtag"]
    myAttrs = ["custattr"]

main :: IO ()
main = hspec $ do
  describe "Sanitized HTML is not changed" $ do
    it "HTML entities should not be escaped" $ do
      test (filterTags safeTags) "text&nbsp;more text" "text&nbsp;more text"
  describe "html sanitizing" $ do
    it "big test" $ do
      let testHTML = " <a href='http://safe.com'>safe</a><a href='unsafe://hack.com'>anchor</a> <img src='evil://evil.com' /> <unsafe></foo> <bar /> <br></br> <b>Unbalanced</div><img src='http://safe.com'>"
      test sanitizeBalance testHTML " <a href=\"http://safe.com\">safe</a><a>anchor</a> <img />   <br /> <b>Unbalanced<div></div><img src=\"http://safe.com\"></b>"
      sanitized testHTML " <a href=\"http://safe.com\">safe</a><a>anchor</a> <img />   <br /> <b>Unbalanced</div><img src=\"http://safe.com\">"

    it "relativeURI" $ do
      let testRelativeURI = "<a href=\"foo\">bar</a>"
      sanitized testRelativeURI testRelativeURI

    it "protocol hack" $
      sanitized "<script src=//ha.ckers.org/.j></script>" ""

    it "object hack" $
      sanitized "<object classid=clsid:ae24fdae-03c6-11d1-8b76-0080c744f389><param name=url value=javascript:alert('XSS')></object>" ""

    it "embed hack" $
      sanitized "<embed src=\"data:image/svg+xml;base64,PHN2ZyB4bWxuczpzdmc9Imh0dH A6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiB4bWxucz0iaHR0cDovL3d3dy53My5vcmcv MjAwMC9zdmciIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hs aW5rIiB2ZXJzaW9uPSIxLjAiIHg9IjAiIHk9IjAiIHdpZHRoPSIxOTQiIGhlaWdodD0iMjAw IiBpZD0ieHNzIj48c2NyaXB0IHR5cGU9InRleHQvZWNtYXNjcmlwdCI+YWxlcnQoIlh TUyIpOzwvc2NyaXB0Pjwvc3ZnPg==\" type=\"image/svg+xml\" AllowScriptAccess=\"always\"></embed>" ""

    it "ucase image hack" $
      sanitized "<IMG src=javascript:alert('XSS') />" "<img />"

  describe "allowedCssAttributeValue" $ do
    it "allows hex" $ do
      assert $ allowedCssAttributeValue "#abc"
      assert $ allowedCssAttributeValue "#123"
      assert $ not $ allowedCssAttributeValue "abc"
      assert $ not $ allowedCssAttributeValue "123abc"

    it "allows rgb" $ do
      assert $ allowedCssAttributeValue "rgb(1,3,3)"
      assert $ not $ allowedCssAttributeValue "rgb()"

    it "allows units" $ do
      assert $ allowedCssAttributeValue "10 px"
      assert $ not $ allowedCssAttributeValue "10 abc"

  describe "css sanitizing" $ do
    it "removes style when empty" $
      sanitized "<p style=''></p>" "<p></p>"

    it "allows any non-url value for white-listed properties" $ do
      let whiteCss = "<p style=\"letter-spacing:foo-bar;text-align:10million\"></p>"
      sanitized whiteCss whiteCss

    it "rejects any url value" $ do
      let whiteCss = "<p style=\"letter-spacing:foo url();text-align:url(http://example.com)\"></p>"
      sanitized whiteCss "<p style=\"letter-spacing:foo \"></p>"

    it "rejects properties not on the white list" $ do
      let blackCss = "<p style=\"anything:foo-bar;other-stuff:10million\"></p>"
      sanitized blackCss "<p></p>"

    it "rejects invalid units for grey-listed css" $ do
      let greyCss = "<p style=\"background:foo-bar;border:10million\"></p>"
      sanitized greyCss "<p></p>"

    it "allows valid units for grey-listed css" $ do
      let grey2Css = "<p style=\"background:1;border-foo:10px\"></p>"
      sanitized grey2Css grey2Css

  describe "balancing" $ do
    it "adds missing elements" $ do
      sanitizedB "<a>foo" "<a>foo</a>"
    it "doesn't add closing voids" $ do
      sanitizedB "<img><hr/>" "<img><hr />"
    it "removes closing voids" $ do
      sanitizedB "<img></img>" "<img />"
    it "interleaved" $
      sanitizedB "<i>hello<b>world</i>" "<i>hello<b>world<i></i></b></i>"

  describe "customized white list" $ do
    it "does not filter custom tags" $ do
      let custtag = "<p><custtag></custtag></p>"
      sanitizedC custtag custtag
    it "filters non-custom tags" $ do
      sanitizedC "<p><weird></weird></p>" "<p></p>"
    it "does not filter custom attributes" $ do
      let custattr = "<p custattr=\"foo\"></p>"
      sanitizedC custattr custattr
    it "filters non-custom attributes" $ do
      sanitizedC "<p weird=\"bar\"></p>" "<p></p>"

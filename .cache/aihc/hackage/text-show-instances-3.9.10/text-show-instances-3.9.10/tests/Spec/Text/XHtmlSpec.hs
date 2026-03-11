{-|
Module:      Spec.Text.XHtmlSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the @xhtml@ library.
-}
module Spec.Text.XHtmlSpec (main, spec) where

import Data.Proxy (Proxy(..))

import Instances.Text.XHtml ()

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)

import TextShow.Text.XHtml ()
import Text.XHtml.Frameset (Html, HtmlAttr, HotLink)
import Text.XHtml.Table (HtmlTable)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Html" $
        matchesTextShowSpec (Proxy :: Proxy Html)
    describe "[Html]" $
        matchesTextShowSpec (Proxy :: Proxy [Html])
    describe "HtmlAttr" $
        matchesTextShowSpec (Proxy :: Proxy HtmlAttr)
    describe "HotLink" $
        matchesTextShowSpec (Proxy :: Proxy HotLink)
    describe "HtmlTable" $
        matchesTextShowSpec (Proxy :: Proxy HtmlTable)

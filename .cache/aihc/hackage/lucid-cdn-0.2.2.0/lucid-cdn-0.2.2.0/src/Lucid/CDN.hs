{-|
  Module     : Lucid.CDN
  License    : MIT
  Stability  : experimental

Curated list of CDN imports for use with Lucid.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Lucid.CDN (
  bootstrapCSS_4_5_2
, bootstrapJS_4_5_2
, bootstrapCSS_5_0_0_alpha1
, bootstrapJS_5_0_0_alpha1
, fontawesome_4_7_0
, jquery_3_5_1
, mathjax_3_1_0
, modernizr_2_8_3
, popper_2_4_4
, sudoslider_3_5_0
, tailwind_1_9_2
) where

import Lucid

_blank :: Html ()
_blank = toHtml ("" :: String)

-- | Bootstrap CSS v4.5.2
bootstrapCSS_4_5_2 :: Monad m => HtmlT m ()
bootstrapCSS_4_5_2 = link_ [
    rel_ "stylesheet"
  , href_ "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css"
  , integrity_ "sha384-JcKb8q3iqJ61gNV9KGb8thSsNjpSL0n8PARn9HuZOnIxN0hoP+VmmDGMN5t9UJ0Z"
  , crossorigin_ "anonymous" ]

-- | Bootstrap JS v4.5.2
bootstrapJS_4_5_2 :: Monad m => HtmlT m ()
bootstrapJS_4_5_2 = script_ [
    src_ "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/js/bootstrap.min.js"
  , integrity_ "sha384-B4gt1jrGC7Jh4AgTPSdUtOBvfO8shuf57BaghqFfPlYxofvL8/KUEfYiJOMMV+rV"
  , crossorigin_ "anonymous" ] _blank

-- | Bootstrap CSS v5.0.0
bootstrapCSS_5_0_0_alpha1 :: Monad m => HtmlT m ()
bootstrapCSS_5_0_0_alpha1 = link_ [
    rel_ "stylesheet"
  , href_ "https://stackpath.bootstrapcdn.com/bootstrap/5.0.0-alpha1/css/bootstrap.min.css"
  , integrity_ "sha384-r4NyP46KrjDleawBgD5tp8Y7UzmLA05oM1iAEQ17CSuDqnUK2+k9luXQOfXJCJ4I"
  , crossorigin_ "anonymous"]

-- | Bootstrap JS v5.0.0
bootstrapJS_5_0_0_alpha1 :: Monad m => HtmlT m ()
bootstrapJS_5_0_0_alpha1 = script_ [
    src_ "https://stackpath.bootstrapcdn.com/bootstrap/5.0.0-alpha1/js/bootstrap.min.js"
  , integrity_ "sha384-oesi62hOLfzrys4LxRF63OJCXdXDipiYWBnvTl9Y9/TRlw5xlKIEHpNyvvDShgf/"
  , crossorigin_ "anonymous"] _blank

-- | FontAwesome v4.7.0
fontawesome_4_7_0 :: Monad m => HtmlT m ()
fontawesome_4_7_0 = link_ [
    rel_ "stylesheet"
  , href_ "https://stackpath.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
  , integrity_ "sha384-wvfXpqpZZVQGK6TAh5PVlGOfQNHSoD2xbE+QkPxCAFlNEevoEH3Sl0sibVcOQVnN"
  , crossorigin_ "anonymous" ]

-- | JQuery V3.5.1
jquery_3_5_1 :: Monad m => HtmlT m ()
jquery_3_5_1 = script_ [
    src_ "https://cdnjs.cloudflare.com/ajax/libs/jquery/3.5.1/jquery.min.js"
  , integrity_ "sha512-bLT0Qm9VnAYZDflyKcBaQ2gg0hSYNQrJ8RilYldYQ1FxQYoCLtUjuuRuZo+fjqhx/qtq/1itJ0C2ejDxltZVFg=="
  , crossorigin_ "anonymous"] _blank

-- | MathJax V3.1.0
mathjax_3_1_0 :: Monad m => HtmlT m ()
mathjax_3_1_0 = script_ [
    src_ "https://cdn.jsdelivr.net/npm/mathjax@3.1.0/es5/tex-mml-chtml.js"
  , integrity_ "sha384-ynpCZozLxgqK3wrgBu8qH8qPG3eD8mME8z0zugAX26UMb5HfLp2PtvtDH4vdmgkm"
  , crossorigin_ "anonymous"] _blank

-- | Modernizr V2.8.3
modernizr_2_8_3 :: Monad m => HtmlT m ()
modernizr_2_8_3 = script_ [
    src_ "https://cdnjs.cloudflare.com/ajax/libs/modernizr/2.8.3/modernizr.min.js"
  , integrity_ "sha512-3n19xznO0ubPpSwYCRRBgHh63DrV+bdZfHK52b1esvId4GsfwStQNPJFjeQos2h3JwCmZl0/LgLxSKMAI55hgw=="
  , crossorigin_ "anonymous"] _blank

-- | Popper V2.4.4
popper_2_4_4 :: Monad m => HtmlT m ()
popper_2_4_4 = script_ [
    src_ "https://cdnjs.cloudflare.com/ajax/libs/popper.js/2.4.4/umd/popper.min.js"
  , integrity_ "sha512-eUQ9hGdLjBjY3F41CScH3UX+4JDSI9zXeroz7hJ+RteoCaY+GP/LDoM8AO+Pt+DRFw3nXqsjh9Zsts8hnYv8/A=="
  , crossorigin_ "anonymous"] _blank

-- SudoSlider V3.5.0
sudoslider_3_5_0 :: Monad m => HtmlT m ()
sudoslider_3_5_0 = script_ [
    src_ "https://cdn.jsdelivr.net/npm/sudoslider@3.5.0/js/jquery.sudoSlider.min.js"
  , integrity_ "sha256-jhgyTeC6IL/fR5R777vflEtlTg9lkxcPjfTVKC0AK2c="
  , crossorigin_ "anonymous"] _blank

-- Tailwind V1.9.2
tailwind_1_9_2 :: Monad m => HtmlT m ()
tailwind_1_9_2 = link_ [
    rel_ "stylesheet"
  , src_ "https://cdnjs.cloudflare.com/ajax/libs/tailwindcss/1.9.2/tailwind.min.css"
  , integrity_ "sha512-l7qZAq1JcXdHei6h2z8h8sMe3NbMrmowhOl+QkP3UhifPpCW2MC4M0i26Y8wYpbz1xD9t61MLT9L1N773dzlOA=="
  , crossorigin_ "anonymous"]

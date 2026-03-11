{-# LANGUAGE OverloadedStrings #-}

module Web.Page.Hastache
  ( module X
  , template
  ) where

import Web.Page.Types as X

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT


-- | We choose to not interpolate the @WebPage@ data type as a JSON Hastache
-- template value because I don't want the portions of @WebPage@ visible by
-- @content@.
template :: WebPage LT.Text T.Text T.Text
         -> LT.Text
         -> LT.Text
template page content = mconcat
  [ "<!DOCTYPE html>"
  , "<html>"
  , "<head>"
  , initScripts page
  , "<title>" <> LT.fromStrict (pageTitle page) <> "</title>"
  , metaVars page
  , favicon page
  , beforeStylesScripts page
  , styles page
  , afterStylesScripts page
  , "</head>"
  , "<body style=\"" <> LT.fromStrict (bodyStyles page) <> "\" >"
  , content
  , bodyScripts page
  , "</body>"
  , "</html>"
  ]

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Defines supporting types.
module Zenacy.HTML.Internal.Types
  ( HTMLNamespace(..)
  , HTMLAttrNamespace(..)
  ) where

import Data.Default
  ( Default(..)
  )

-- | An HTML namespace type.
data HTMLNamespace
  = HTMLNamespaceHTML
  | HTMLNamespaceSVG
  | HTMLNamespaceMathML
    deriving (Eq, Ord, Show)

-- | An HTML attribute namespace type.
data HTMLAttrNamespace
  = HTMLAttrNamespaceNone
  | HTMLAttrNamespaceXLink
  | HTMLAttrNamespaceXML
  | HTMLAttrNamespaceXMLNS
    deriving (Eq, Ord, Show)

-- | Defines a default namespace.
instance Default HTMLNamespace where
  def = HTMLNamespaceHTML

-- | Defines a default attribute namespace.
instance Default HTMLAttrNamespace where
  def = HTMLAttrNamespaceNone

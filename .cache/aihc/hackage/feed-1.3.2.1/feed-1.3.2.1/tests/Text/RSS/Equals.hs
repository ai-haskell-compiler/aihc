{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.RSS.Equals where

import Prelude.Compat

import Text.RSS.Syntax (RSSCloud(..))

instance Eq RSSCloud where
  (RSSCloud rssCloudDomain1 rssCloudPort1 rssCloudPath1 rssCloudRegisterProcedure1 rssCloudProtocol1 rssCloudAttrs1) == (RSSCloud rssCloudDomain2 rssCloudPort2 rssCloudPath2 rssCloudRegisterProcedure2 rssCloudProtocol2 rssCloudAttrs2) =
    (rssCloudDomain1 == rssCloudDomain2) &&
    (rssCloudPort1 == rssCloudPort2) &&
    (rssCloudPath1 == rssCloudPath2) &&
    (rssCloudRegisterProcedure1 == rssCloudRegisterProcedure2) &&
    (rssCloudProtocol1 == rssCloudProtocol2) && (rssCloudAttrs1 == rssCloudAttrs2)

{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module OpenSuse.Types.ProjectId
  ( ProjectId, mkProjectId, unProjectId
  )
  where

import OpenSuse.Prelude
import OpenSuse.Prelude.Parser as Parse
import OpenSuse.Prelude.PrettyPrinting as Pretty

import Data.Aeson as Json
import Data.Aeson.Types as Json ( toJSONKeyText )

-- | Projects are identified on OBS by a string path.
--
-- >>> parse "project id" "SUSE:SLE-12-SP2:Update" :: ProjectId
-- ProjectId ["SUSE","SLE-12-SP2","Update"]
-- >>> parseM "project id" "SUSE::SLE-12-SP2" :: Maybe ProjectId
-- Nothing
-- >>> parseM "project id" ":SUSE" :: Maybe ProjectId
-- Nothing
-- >>> parseM "project id" "SUSE:" :: Maybe ProjectId
-- Nothing

newtype ProjectId = ProjectId [String]
  deriving (Show, Eq, Ord, Generic, Hashable, Binary, NFData, Semigroup, Monoid)

-- | Constructor function for project identifiers.
--
-- TODO: Figure out how to deal with the [] project.
mkProjectId :: [String] -> ProjectId
mkProjectId = ProjectId

-- | Accessor function for the underlying path of strings.
unProjectId :: ProjectId -> [String]
unProjectId (ProjectId str) = str

instance IsString ProjectId where
  fromString = parse "code stream"

instance Pretty ProjectId where
  pPrint = hcat . punctuate colon . map text . unProjectId

instance HasParser ProjectId where
  parser = mkProjectId <$> sepBy1 (many1 (alphaNum <|> oneOf "_-")) (Parse.char ':')

instance FromJSON ProjectId where
  parseJSON = withText "ProjectId" (parseM "project id")

instance FromJSONKey ProjectId where
  fromJSONKey = FromJSONKeyTextParser (parseM "project id")

instance ToJSON ProjectId where
  toJSON = fromString . prettyShow

instance ToJSONKey ProjectId where
  toJSONKey = toJSONKeyText (fromString . prettyShow)

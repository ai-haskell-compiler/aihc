{-# LANGUAGE ScopedTypeVariables #-}
module Data.Yaml.Frontmatter where

import           Data.Attoparsec.ByteString
import           Data.Frontmatter.Internal
import           Data.Yaml                  (FromJSON, Value, decodeEither)

-- |
-- Parses a YAML frontmatter or JSON frontmatter from a 'ByteString' as a
-- 'Value'. Because of how @Data.Yaml@ is implemented using @aeson@, this will
-- succeed for JSON frontmatters as well as YAML ones.
frontmatterYaml :: FromJSON a => Parser a
frontmatterYaml = frontmatterYaml' <?> "frontmatterYaml"
  where
    frontmatterYaml' = do
        f <- frontmatter
        case decodeEither f of
            Left e -> fail e
            Right v -> return v

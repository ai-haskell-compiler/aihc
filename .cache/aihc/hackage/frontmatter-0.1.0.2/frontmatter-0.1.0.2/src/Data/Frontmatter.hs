{-# LANGUAGE ScopedTypeVariables #-}
module Data.Frontmatter
    ( -- * Frontmatter parser
      frontmatter
    , frontmatterYaml
      -- * Utility functions
    , parseFrontmatter
    , parseFrontmatterMaybe
    , parseFrontmatterEither
    , parseYamlFrontmatter
    , parseYamlFrontmatterMaybe
    , parseYamlFrontmatterEither
      -- * Re-exports
    , parse
    , maybeResult
    , eitherResult
    , Parser
    , Result
    , IResult(..)
    )
  where

import           Data.Attoparsec.ByteString
import           Data.ByteString            (ByteString)
import           Data.Frontmatter.Internal
import           Data.Yaml                  (FromJSON)
import           Data.Yaml.Frontmatter

-- |
-- Parse a frontmatter from a 'ByteString' returning a 'Result'. Just extracts
-- whatever is on the frontmatter; doesn't care what it is.
parseFrontmatter :: ByteString -> Result ByteString
parseFrontmatter = parse frontmatter

-- |
-- 'parseFrontmatter' but returning a 'Maybe'
parseFrontmatterMaybe :: ByteString -> Maybe ByteString
parseFrontmatterMaybe = maybeResult . parse frontmatter

-- |
-- 'parseFrontmatter' but returning an 'Either'
parseFrontmatterEither :: ByteString -> Either String ByteString
parseFrontmatterEither = eitherResult . parse frontmatter

-- |
-- Parse a frontmatter from a 'ByteString' returning a 'FromJSON a'. Will parse
-- both JSON and YAML.
parseYamlFrontmatter :: FromJSON a => ByteString -> Result a
parseYamlFrontmatter = parse frontmatterYaml

-- |
-- 'parseYamlFrontmatter' but returning a 'Maybe'
parseYamlFrontmatterMaybe :: FromJSON a => ByteString -> Maybe a
parseYamlFrontmatterMaybe = maybeResult . parse frontmatterYaml

-- |
-- 'parseYamlFrontmatter' but returning an 'Either'
parseYamlFrontmatterEither :: FromJSON a => ByteString -> Either String a
parseYamlFrontmatterEither = eitherResult . parse frontmatterYaml

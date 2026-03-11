{-# LANGUAGE OverloadedStrings #-}
module Data.Frontmatter.Internal where

import           Control.Applicative              ((*>))
import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Char8            as ByteString (pack)

-- |
-- A parser for a frontmatter; returns it as a 'ByteString'. Doesn't fail even
-- if it's empty. When it fails; returns a a IResult with the whole input
-- rather than consuming it.
frontmatter :: Parser ByteString
frontmatter = frontmatter' <?> "frontmatter"
  where
    frontmatter' = do
        f <- frontmatterSeparator *> manyTill anyChar frontmatterSeparator
        return $ ByteString.pack f

-- |
-- Internal parser for the frontmatter separator
frontmatterSeparator :: Parser ()
frontmatterSeparator = string "---" >> endOfLine

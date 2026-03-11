-- | This module introduces 'ConduitParser', a wrapper around 'Sink' that behaves like a parser.
--
-- You probably want to import the "Text.Parser.Combinators" module together with this module.
module Data.Conduit.Parser
  ( -- * Conduit parser monad
    ConduitParser()
  , runConduitParser
  , named
    -- * Primitives
  , await
  , leftover
  , getParserNames
  , getParserName
    -- * Utility
  , peek
    -- * Exception
  , ConduitParserException(..)
  ) where

import           Data.Conduit.Parser.Internal

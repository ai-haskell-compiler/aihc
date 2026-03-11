{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Newtype wrappers for parsers

module Text.Parser.Wrapper where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Text.Parser.Combinators (Parsing)
import Text.Parser.LookAhead (LookAheadParsing)
import Text.Parser.Char (CharParsing)
import Text.Parser.Token (TokenParsing)

-- | Wrapper that signifies lazy 'Data.ByteString.Lazy.ByteString' inputs
newtype Lazy   f a = Lazy{getLazy :: f a} deriving (Eq, Ord, Read, Show,
                                                    Functor, Applicative, Alternative,
                                                    Monad, MonadPlus,
                                                    Parsing, LookAheadParsing, CharParsing, TokenParsing)
-- | Wrapper that signifies strict 'Data.ByteString.ByteString' inputs
newtype Strict f a = Strict{getStrict :: f a} deriving (Eq, Ord, Read, Show,
                                                        Functor, Applicative, Alternative,
                                                        Monad, MonadPlus,
                                                        Parsing, LookAheadParsing, CharParsing, TokenParsing)

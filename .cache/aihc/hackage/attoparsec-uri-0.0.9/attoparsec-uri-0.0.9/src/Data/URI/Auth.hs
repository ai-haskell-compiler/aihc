{-# LANGUAGE
    RecordWildCards
  , OverloadedStrings
  , DeriveGeneric
  , DeriveDataTypeable
  #-}

module Data.URI.Auth where

import Data.URI.Auth.Host (URIAuthHost, parseURIAuthHost, printURIAuthHost)

import Prelude hiding (Maybe (..), maybe)
import qualified Prelude as P
import Data.Strict.Maybe (Maybe (..), maybe)
import Data.Text (Text)
import Data.Word (Word16)
import qualified Data.Text as T
import Data.Attoparsec.Text ( Parser, char, decimal, takeWhile1, (<?>))
import Control.Monad (void)
import Control.Applicative (optional)

import Data.Data (Typeable)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof, listOf1, elements)


data URIAuth = URIAuth
  { uriAuthUser     :: !(Maybe Text) -- ^ a designated user - @ssh://git\@github.com@ is @git@
  , uriAuthPassword :: !(Maybe Text) -- ^ a designated password (this field is depricated in RFC 3986, passwords with an at-character will not parse) - @https://user:password\@github.com@ is @password@
  , uriAuthHost     :: !URIAuthHost
  , uriAuthPort     :: !(Maybe Word16) -- ^ the port, if it exists - @foobar.com:3000@ is @3000@ as a 16-bit unsigned int.
  } deriving (Show, Eq, Typeable, Generic)

instance Arbitrary URIAuth where
  arbitrary = do
    mUser <- arbitraryUser
    mPassword <- if mUser == Nothing then pure Nothing else Just <$> arbitraryNonEmptyText
    host <- arbitrary
    mPort <- arbitraryPort
    pure (URIAuth mUser mPassword host mPort)
    where
      arbitraryUser = oneof [pure Nothing, Just <$> arbitraryNonEmptyText]
      arbitraryPort = oneof [pure Nothing, Just <$> arbitrary]
      arbitraryNonEmptyText = T.pack <$> listOf1 (elements ['a' .. 'z'])


-- | Prints the URI auth but omits the password even if present.
printURIAuth :: URIAuth -> Text
printURIAuth URIAuth{..} =
     ( case uriAuthPassword of
         Nothing -> maybe "" (<> "@") uriAuthUser
         Just p -> maybe ":" (<> ":") uriAuthUser <> p <> "@"
     )
  <> printURIAuthHost uriAuthHost
  <> maybe "" (\p -> ":" <> T.pack (show p)) uriAuthPort


parseURIAuth :: Parser URIAuth
parseURIAuth = do
  let withPassword = do
        void (char ':')
        parsePassword
      hasUsernameOrPassword = do
        u <- Just <$> parseUser
        p <- toStrictMaybe <$> optional withPassword
        void (char '@')
        pure (u,p)
  (u,p) <- do
    mUP <- optional hasUsernameOrPassword
    case mUP of
      P.Nothing -> pure (Nothing, Nothing)
      P.Just xs -> pure xs
  URIAuth u
          p
          <$> parseURIAuthHost
          <*> (toStrictMaybe <$> optional parsePort)
  where
    parseUser = takeWhile1 (\c -> c `notElem` ['@','.',':','/','?','&','=','[']) <?> "user value"
    parsePassword = takeWhile1 (\c -> c `notElem` ['@','.',':','/','?','&','=','[']) <?> "password value"
    parsePort = do
      void (char ':') <?> "port delimiter"
      decimal

    toStrictMaybe P.Nothing = Nothing
    toStrictMaybe (P.Just x) = Just x

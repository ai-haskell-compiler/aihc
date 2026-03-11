module AesonValueParser.Error where

import AesonValueParser.Prelude
import qualified Data.Text as Text

data Error
  = Error
      [Text]
      -- ^ Path
      Text
      -- ^ Message

instance Semigroup Error where
  (<>) _ b = b

instance Monoid Error where
  mempty = Error [] ""
  mappend = (<>)

instance IsString Error where
  fromString = message . fromString

instance Show Error where
  show = Text.unpack . toText

{-# INLINE indexed #-}
indexed :: Int -> Error -> Error
indexed = named . fromString . show

{-# INLINE named #-}
named :: Text -> Error -> Error
named name (Error path message) = Error (name : path) message

{-# INLINE message #-}
message :: Text -> Error
message = Error []

toText :: Error -> Text
toText (Error path message) =
  "AST parsing error at path "
    <> foldMap (\x -> "/" <> x) path
    <> ": "
    <> message

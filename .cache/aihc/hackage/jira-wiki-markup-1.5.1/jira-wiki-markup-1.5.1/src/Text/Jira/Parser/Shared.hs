{-# LANGUAGE LambdaCase #-}
{-|
Module      : Text.Jira.Parser.Shared
Copyright   : © 2019–2023 Albert Krewinkel
License     : MIT

Maintainer  : Albert Krewinkel <tarleb@zeitkraut.de>
Stability   : alpha
Portability : portable

Parsers whch are shared between multiple modules.
-}
module Text.Jira.Parser.Shared
  ( icon
  , icon'
  , colorName
  ) where

import Data.Char (isLetter)
import Data.Text (Text)
import Text.Jira.Markup
import Text.Parsec

-- | Parses an icon
icon :: Parsec Text u Icon
icon = icon' <* notFollowedBy alphaNum

-- | Like 'icon', but doesn't check whether the sequence is followed by
-- a character that would prevent the interpretation as an icon.
icon' :: Parsec Text u Icon
icon' = smiley <|> otherIcon

smiley :: Parsec Text u Icon
smiley = try $ choice
  [ IconWinking <$ string ";)"
  , char ':' *> anyChar >>= \case
      'D' -> pure IconSmiling
      ')' -> pure IconSlightlySmiling
      '(' -> pure IconFrowning
      'P' -> pure IconTongue
      c   -> fail ("unknown smiley: :" ++ [c])
  ]

otherIcon :: Parsec Text u Icon
otherIcon = try $ do
  let isIconChar c = isLetter c || (c `elem` ("/!+-?*" :: String))
  name <- char '('
          *> many1 (satisfy isIconChar)
          <* char ')'
  case name of
    "y"       -> pure IconThumbsUp
    "n"       -> pure IconThumbsDown
    "i"       -> pure IconInfo
    "/"       -> pure IconCheckmark
    "x"       -> pure IconX
    "!"       -> pure IconAttention
    "+"       -> pure IconPlus
    "-"       -> pure IconMinus
    "?"       -> pure IconQuestionmark
    "on"      -> pure IconOn
    "off"     -> pure IconOff
    "*"       -> pure IconStar
    "*r"      -> pure IconStarRed
    "*g"      -> pure IconStarGreen
    "*b"      -> pure IconStarBlue
    "*y"      -> pure IconStarYellow
    "flag"    -> pure IconFlag
    "flagoff" -> pure IconFlagOff
    _         -> fail ("not a known emoji" ++ name)

colorName :: Parsec Text u String
colorName = many1 letter <|> hexColor
  where
    hexColor = (:) <$> option '#' (char '#') <*> count 6 hexDigit

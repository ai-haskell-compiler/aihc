module YamlUnscrambler.Util.Maybe where

import YamlUnscrambler.Prelude hiding (lookup)

firstNonEmpty :: Maybe a -> Maybe a -> Maybe a
firstNonEmpty a b =
  case a of
    Just a -> Just a
    _ -> b

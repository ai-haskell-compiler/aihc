module YamlUnscrambler.Util.List where

firstNonEmpty :: [a] -> [a] -> [a]
firstNonEmpty a b =
  case a of
    [] -> b
    _ -> a

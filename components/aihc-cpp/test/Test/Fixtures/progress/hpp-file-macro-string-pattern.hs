{-# LANGUAGE LambdaCase #-}

f = \case
  "__FILE__" -> "literal __FILE__ should stay untouched"
  x -> x

g = __FILE__

module Data.Char.Small where

{- | non-total function -}
superscript :: Char -> Char
superscript =
   maybe (error "no superscript character available") id .
   superscriptMaybe

superscriptMaybe :: Char -> Maybe Char
superscriptMaybe c =
   case c of
      '0' -> Just '\x2070'
      '1' -> Just '\xb9'
      '2' -> Just '\xb2'
      '3' -> Just '\xb3'
      '4' -> Just '\x2074'
      '5' -> Just '\x2075'
      '6' -> Just '\x2076'
      '7' -> Just '\x2077'
      '8' -> Just '\x2078'
      '9' -> Just '\x2079'

      '+' -> Just '\x207A'
      '-' -> Just '\x207B'
      '=' -> Just '\x207C'
      '(' -> Just '\x207D'
      ')' -> Just '\x207E'

      'i' -> Just '\x2071'
      'n' -> Just '\x207F'

      _ -> Nothing


{- | non-total function -}
subscript :: Char -> Char
subscript =
   maybe (error "no subscript character available") id .
   subscriptMaybe

subscriptMaybe :: Char -> Maybe Char
subscriptMaybe c =
   case c of
      '0' -> Just '\x2080'
      '1' -> Just '\x2081'
      '2' -> Just '\x2082'
      '3' -> Just '\x2083'
      '4' -> Just '\x2084'
      '5' -> Just '\x2085'
      '6' -> Just '\x2086'
      '7' -> Just '\x2087'
      '8' -> Just '\x2088'
      '9' -> Just '\x2089'

      '+' -> Just '\x208A'
      '-' -> Just '\x208B'
      '=' -> Just '\x208C'
      '(' -> Just '\x208D'
      ')' -> Just '\x208E'

      'a' -> Just '\x2090'
      'e' -> Just '\x2091'
      'o' -> Just '\x2092'
      'x' -> Just '\x2093'

      'h' -> Just '\x2095'
      'k' -> Just '\x2096'
      'l' -> Just '\x2097'
      'm' -> Just '\x2098'
      'n' -> Just '\x2099'
      'p' -> Just '\x209A'
      's' -> Just '\x209B'
      't' -> Just '\x209C'

      _ -> Nothing

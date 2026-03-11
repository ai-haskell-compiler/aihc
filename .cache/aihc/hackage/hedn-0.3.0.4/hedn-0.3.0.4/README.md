# Haskell EDN

An EDN parsing and encoding library.

Based on [spec] and [hints] published on GitHub.

[spec]: https://github.com/edn-format/edn
[hints]: https://github.com/wagjo/serialization-formats

Hackage: https://hackage.haskell.org/package/hedn

Stackage: https://www.stackage.org/package/hedn

Tutorial: https://dpwiz.gitlab.io/hedn

# Examples

## AST

```clojure
(#haskell/edn example/kitchensink ; tagged symbol
 nil ; ugh..
 \space ; character
 "dynamic \"typing\"" ; string
 {:numbers ; keyword
  [42 ; integer
   42.0 ; floating
  ] ; Vector
  :bools
  #{true, false} ; Set (with optional commas)
 } ; Map
) ; List
```

```haskell
import qualified Data.EDN as EDN
import qualified Data.Text.IO as Text

main = do
  edn <- Text.readFile "example.edn"
  either error print $ EDN.parseText "example.edn" edn
```

```haskell
NoTag
  (List
     [ Tagged "haskell" "edn" (Symbol "example" "kitchensink")
     , NoTag Nil
     , NoTag (Character ' ')
     , NoTag (String "dynamic \"typing\"")
     , NoTag
         (Map
            (fromList
               [ ( NoTag (Keyword "bools")
                 , NoTag
                     (Set (fromList [ NoTag (Boolean False) , NoTag (Boolean True) ]))
                 )
               , ( NoTag (Keyword "numbers")
                 , NoTag (Vec [ NoTag (Integer 42) , NoTag (Floating 42.0) ])
                 )
               ]))
     ])
```

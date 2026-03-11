## 1.25.14 (2024-12-12)
- fix inaccuracies in haskell generation code (#17 from @SiriusCourses):
  - Fix mkNamespace for case when QName is recognized at root element
  - Fix QName parser to recognize names with namespaces
  - Prevent multiple declarations after type lifting hoist
  - Inject groups from reference at topElementDecl
  - Fix bug unexcaped module name in generated haskell code
  - Add to Environment deriving for Show and Eq for simpler debug
  - Fix name with dashes for simpleType
- PrettyH*: Add missing pattern match to ppComment (#24 from @e-rk)
- Allow base-4.20 and filepath-1.5; bump CI to GHC 9.8 (#21, @andreasabel)
- allow base-4.22 for ghc-9.12 (#23, @juhp)

## 1.25.13 (2023-07-13)
- correct zero time duration P0S to PT0S (@dten, #16)
- allow ghc-9.6 base (@andreasabel, #15)
- allow bytestring-0.12 (@andreasabel, #18)

## 1.25.12 (2022-10-18)
- Allow parsing empty strings, avoiding space leak #13 (Teo Camarasu)

## 1.25.11 (2022-09-13)
- revert "allow empty text content #10" to avoid haxr memory leak

## 1.25.10 (2022-09-12)
- better pretty printer formatting #8 (Alexander Vieth)
- allow building with GHC 9.4 #9 (Andreas Abel)
- allow empty text content #10 (Teo Camarasu)

## 1.25.9 (2022-04-10)
- fix 1.25.7 regression in Xtract.Parse (#7 by Isaac van Bakel)
- comment typo fixes (#6 by Eric Lindblad)
- include README file

## 1.25.8 (2021-11-22)
- version the License tag in HaXml.cabal as LGPL-2.1 (#3)
- allow building with ghc 9.2 (#4)

## 1.25.7 (2021-10-15)
- many hlint fixes
- fix the rendering of time durations (#1)

## 1.25.6 (2021-09-26)

- GHC 8.8, 8.10, and 9.0 compatibility

## 1.25.5 (2018-10-30)

- GHC-8.4 and GHC-8.6 compatibility
  - Monoids have Semigroup instances

### 0.3.2 [2024.04.20]
* Add `Data.Proxyless.RequiredTypeArguments`, a variant of `Data.Proxyless`
  which uses `RequiredTypeArguments` instead of `TypeApplications`.

### 0.3.1 [2019.02.20]
* Make the build warning-free under `-Wcompat`.

## 0.3
* Update for GHC 8.2
  * Since `typeRep#`, `typeNatTypeRep`, and `typeSymbolTypeRep` are no longer
    exported from `base`, `theTypeRep#`, `theTypeNatTypeRep`, and
    `theTypeSymbolTypeRep` are now synonyms for `theTypeRep`
  * Happily, the new type signature for `GHC.OverloadedLabels.fromLabel` is now
    exactly the same as `theFromLabel`, so the latter is now a synonym for the
    former
* Use explicit kind variable binders in `Data.Proxyless`
* Use explicit `forall`s in `Data.Proxied` for consistency

## 0.2
* Added the `Data.Proxyless` module
* Added `proxyHashed` to `Data.Proxied`

### 0.1.1
* Enabled `Safe`

## 0.1
* Initial commit

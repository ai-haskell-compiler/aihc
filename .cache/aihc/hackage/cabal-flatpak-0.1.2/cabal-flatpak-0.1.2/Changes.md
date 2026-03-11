# Change log for the `cabal-flatpak` package

## 0.1.1

 * reduce file size of manifests using inlined script `revise.sh`.


## 0.1

 * No need to specify `main-sources` field anymore.

 * Fix Cabal revisions in the imported packages.
   Manually fetch necessary information from `01-index.tar`
   and downloaded packages.

## 0.0

 * First experimental version inspired by `stackpak`.

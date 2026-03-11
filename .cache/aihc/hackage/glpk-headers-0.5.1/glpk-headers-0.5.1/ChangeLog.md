# Changelog for `glpk-headers`

## [Unreleased]

- The `mkHaskellErrorHook` function was added to
  `Math.Programming.Glpk.Headers` to support writing GLPK error
  handlers.

- The `mkHaskellTermHook` function was added to
  `Math.Programming.Glpk.Headers` to support writing GLPK terminal
  callback hooks.

- The `mkHaskellMIPCallback` function was added to
  `Math.Programming.Glpk.Headers` to support writing MIP callback
  functions.

## [0.5.0] -- 2021-03-07

* Switch from Stack to Cabal (and Nix).
* Update to GHC 8.8.4

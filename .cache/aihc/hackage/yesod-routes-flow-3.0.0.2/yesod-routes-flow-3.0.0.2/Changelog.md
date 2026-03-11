## [_Unreleased_](https://github.com/freckle/yesod-routes-flow/compare/v3.0.0.2...main)

None

## [v3.0.0.2](https://github.com/freckle/yesod-routes-flow/compare/v3.0.0.1...v3.0.0.2)

- Support GHCs 9.0 and 9.2

## [v3.0.0.1](https://github.com/freckle/yesod-routes-flow/compare/2.0...v3.0.0.1)

- Setup CI/CD
- Starting in `yesod-core-1.6.2`, `yesod-core` started deriving `Show` for `ResourceTree` and `FlatResource`. To prevent duplicate instance errors, this package now only derives `Show` for `yesod-core < 1.6.2`.
- An implication of this is that building anything less than `yesod-routes-flow-3.0.0.1` with `yesod-core-1.6.2` will cause duplicate instance compiler errors.

# Tagged Identity

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/tagged-identity.svg?style=flat)](https://hackage.haskell.org/package/tagged-identity)
[![Stackage Nightly](http://stackage.org/package/tagged-identity/badge/nightly)](http://stackage.org/nightly/package/tagged-identity)
[![Stackage LTS](http://stackage.org/package/tagged-identity/badge/lts)](http://stackage.org/lts/package/tagged-identity)
[![CI](https://github.com/mrkkrp/tagged-identity/actions/workflows/ci.yaml/badge.svg)](https://github.com/mrkkrp/tagged-identity/actions/workflows/ci.yaml)

The library provides a monad transformer that works just like `IdentityT`,
but can be tagged at the type level. This allows us to work with monad
stacks as usual, except we can make two identical monad stacks to have
different types. The main application for this is, of course, the ability to
have different instances for otherwise identical stacks without having to do
`newtype` wrapping.

## Contribution

Issues, bugs, and questions may be reported in [the GitHub issue tracker for
this project](https://github.com/mrkkrp/tagged-identity/issues).

Pull requests are also welcome.

## License

Copyright © 2016–present Mark Karpov

Distributed under BSD 3 clause license.

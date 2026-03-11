* Hackage: <http://hackage.haskell.org/package/FloatingHex>
* GitHub:  <http://github.com/LeventErkok/FloatingHex/>

* Latest Hackage released version: 0.5, 2020-09-05

### Version 0.5, 2020-09-05
  * Changes to make it work with GHC 8.10.2

### Version 0.4, 2017-01-15

  * Export the new FloatingHexReader class. Useful when used as an API.

### Version 0.3, 2017-01-15

  * Bump up template-haskell dependency to >= 2.10. As noted by Herbert Valerio Riedel,
    FloatingHex fails to compile with older versions.

  * Make the double->float conversions more robust, by avoiding the rational route.
    (Avoids issues in https://ghc.haskell.org/trac/ghc/ticket/3676)

### Version 0.2, 2017-01-14

  * Support for parsing nan/infinity values
  * Make the printer compliant with printf %a modifier in C

### Version 0.1, 2017-01-14

  * First implementation. The quasiquoter and the pretty-printer are implemented.

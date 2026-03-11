## 1.2.2

* Fix build with hspec >= 2.10

## 1.2.1

* Fix build with hspec >= 2.5
* move to lts-12.9

## 1.2.0

*   The way capabilities are handled was simplified.  In the old API (1.1.0 and below) there was a typeclass TestCapabilities
    and you had to create an enumeration (or use the default provided by this library) and then there was a Using typeclass that
    allowed lists or a single capability.  All this complexity and abuse of typeclasses turned out to be unnecessary.  After 
    using hspec-webdriver in my own projects for several years, the best way to handle capabilities is in a utility module to have
    a list `allBrowsers :: [Capabilities]` and then each session in the spec just passes `allBrowsers` to `using` (the XKCD exaple
    in the documentation now uses this approach).  The `allBrowsers` value in the utility module can then be edited and changed to
    specify which browsers/caps to use for the examples.

    The actual API changes are as follows:

    * TestCapabilities was removed.
    * BrowserDefaults was removed and instead there are symbols `firefoxCaps`, `chromeCaps`, etc. which you can
      use instead of BrowserDefaults.  Or you can ignore these and create your own capaiblities directly using
      utility functions from the `webdriver` package.
    * The Using typeclass was removed but the `using` helper function still exists as a standalone function.
      You must now pass list of capabilities to `using` but otherwise the usage is the same.  As mentioned above,
      I suggest the argument to `using` is a list defined in a utility module similar to the XKCD example.
      (In my large test code, no call to using needed to be changed).
    * `session` and `sessionWith` changed to take a list of Capabilities, but when used with `using` the output of `using`
      is still the input to `session`.  Also, `sessionWith` takes a descriptive string to be used to better describe
      capabilities.

* In previous versions, as soon as an example had an error the entire session was aborted.  This is still the default,
  but there are now functions `runWDOptions` and `runWDWithOptions` which take a setting which can cause the session to continue
  even if the example has an error.  There is an API change in the definition of the `WdExample` data constructor to take the
  new options, but as long as you were using `runWD` and `runWDWith` and not using `WdExample` directly, there is no change
  needed to your code.  `runWD` and `runWDWith` still have the same behavior as before; as soon as an error occurs, the entire
  session is aborted.

## 1.1.0

* Support for webdriver 0.8.  The API in hspec-webdriver itself did not change, but we re-export WebDriver.Commands so I bumped the
  major version. In fact, there were only a few minor changes in the Commands module (most of the changes in webdriver
  were to session management which is handled internally), so it is likely that your test suite will compile without changes.

## 1.0.3

* Allow newer versions of hspec and HUnit

## 1.0.2

* Fix so that the package builds against webdriver 0.6.1

## 1.0.1

* Increase upper bound on hspec to allow hspec 2.1

## 1.0.0

* Update to hspec2.  All the same features are present but some of the types changed
  to reflect hspec2 types.

## 0.3.2

* Update to the new hspec 1.12 API

## 0.3.1

* Convert inspectSession to turn into a pending example instead of throwing an error.

## 0.3.0

* Add support for testing multiple sessions at once
    * multiSession, multiSessionWith, and runWDWith are the new functions
    * the type of runWD and WDExample changed

* Update to hspec 0.11 and webdriver 0.6
    * changed sessionOn to sessionWith to work with new webdriver WDConfig
    * TestCapabilities(newCaps) changed type to @c -> IO W.Capabilities@

## 0.2.3

* Add inspectSession to assist debugging the test suite

## 0.2.2

* Allow newer version of hspec

## 0.2.1

* Allow newer version of hspec

## 0.2.0

* Convert to use webdriver sessions

## 0.1.0

* Initial Release

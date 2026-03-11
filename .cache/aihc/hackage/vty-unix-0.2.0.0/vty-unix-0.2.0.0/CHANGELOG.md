
0.2.0.0
=======

API changes:
* The `settingColorMode` field of `UnixSettings` was removed in favor of
  Vty 6.1's new `configPreferredColorMode` field of the `VtyUserConfig`
  type. This package now uses that setting if present; otherwise it does
  the same color mode detection that it did before this release.

0.1.0.0
=======

Initial release.

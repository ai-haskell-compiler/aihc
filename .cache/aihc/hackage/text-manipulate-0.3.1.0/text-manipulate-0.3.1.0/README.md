# Text Manipulate

[![MPL2][license-badge]][license]
[![Build][build-badge]][build]
[![Hackage][hackage-badge]][hackage]
[![Nix][nix-badge]][nix]
[![Cachix][cachix-badge]][cachix]

[license]: https://opensource.org/licenses/MPL-2.0
[license-badge]: https://img.shields.io/badge/license-MPL%202.0-blue.svg
[build]: https://github.com/brendanhay/text-manipulate/actions
[build-badge]: https://github.com/brendanhay/text-manipulate/workflows/build/badge.svg
[hackage]: http://hackage.haskell.org/package/text-manipulate
[hackage-badge]: https://img.shields.io/hackage/v/text-manipulate.svg
[nix]: https://nixos.org
[nix-badge]: https://img.shields.io/badge/builtwith-nix-purple.svg
[cachix]: https://amazonka.cachix.org
[cachix-badge]: https://img.shields.io/badge/cachix-amazonka-purple.svg

Manipulate identifiers and structurally non-complex pieces
of text by delimiting word boundaries via a combination of whitespace,
control-characters, and case-sensitivity.

Has support for common idioms like casing of programmatic variable names,
taking, dropping, and splitting text by word, and modifying the first character
of a piece of text.

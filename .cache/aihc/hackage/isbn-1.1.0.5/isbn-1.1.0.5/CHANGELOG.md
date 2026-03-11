# Revision history for `isbn`

## 1.1.0.5 - December 26, 2023

- Bump version of `text` dependency to include versions up to 2.2 (exclusive)
- Add `flake.nix` file for Nix Flake-based development (via `nix develop`)
- Bump `nixpkgs.json` to `release-23.11` branch of Nixpkgs for `nix-shell` based development

## 1.1.0.4 - September 6, 2022

- Bumped version bound of `text` dependency to include `text-2.0.x`.

## 1.1.0.3 - January 10, 2022

- Changed imports of `Data.Text` to be qualified to prevent ambiguous import issues caused by changed to `Prelude` in GHC 9.x.

## 1.1.0.2 - November 13, 2020

- Built documentation using GHC 8.8.4
- Changed Nix shell development environment to use `fetchFromGitHub` instead of `fetchGit` for nixpkgs
- Changed `hackage-build` Makefile target to split `sdist` and `docs` outputs into separate subdirectories of `build/`
- Added `hackage-upload` and `hackage-upload-publish` Makefile targets to upload new package builds to Hackage

## 1.1.0.1 - July 30, 2020

- Changed implementation of `validateISBN` to make a copy of the input text, preventing space leaks
- Added some test cases for ISBN-13s with a '979' prefix

## 1.1.0.0 - July 28, 2020

- Changed exports in `Data.ISBN` module to not export the constructors of the `ISBN` data type
  - If constructors are needed, the `Data.Types.ISBN` module can be imported instead
- Added `isISBN10` and `isISBN13` functions to allow for checking of the opaque `ISBN` data type without the use of its value constructors

## 1.0.0.0 - May 17, 2020

- Initial release. Features:
  - General ISBN validation based off of text input, as well as specific ISBN-10 or ISBN-13 validation
  - Validation errors with human-friendly renderers, that can be used to display error messages to end users
  - ISBN-10 to ISBN-13 conversion and vise versa

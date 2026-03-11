## [0.3.0.1] - 2023-10-12

- Fix broken links to ECMA-48 standard

## [0.3.0] - 2023-09-24

- Switch the underlying text builder from `text-builder` to `text-builder-linear`. This also affects the API provided by
  `Text.Builder.ANSI`; users that were relying on the `Buidler` type from `text-builder` specificially are encouraged
  to simply use older versions of this package until they are able to update their own code to use `text-builder-linear`
  instead.

  The motivation for this change is performance and encoraging the ecosystem to move in the right direction. See the
  `text-builder-linear` project for more details. In a nutshell, it is the superior builder type ;)

## [0.2.1.1] - 2023-05-11

- [#6](https://github.com/awkward-squad/text-ansi/pull/6) Fix Windows linker errors on GHC 9.4.5+

## [0.2.1] - 2022-11-05

- Add `String.ANSI` module

## [0.2.0] - 2022-10-28

- Drop the `Data` prefix from `Data.Text.ANSI` and `Data.Text.Builder.ANSI` modules
- Rename `Text.Builder.ANSI` to `Text.Lazy.Builder.ANSI`
- Add `Text.Builder.ANSI` back, but based on `text-builder`'s builder instead of `text`'s builder

## [0.1.1] - 2021-01-07

- Add `Data.Text.Builder.ANSI` module

## [0.1.0.2] - 2020-12-26

- Fix bug in `italic`

## [0.1.0.1] - 2020-06-14

- Relax `base` bounds
- Drop `text-builder` dependency

## [0.1.0] - 2018-11-14

- Initial release

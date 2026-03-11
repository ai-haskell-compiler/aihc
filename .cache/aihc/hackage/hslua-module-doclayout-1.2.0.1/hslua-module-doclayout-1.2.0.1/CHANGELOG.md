# Changelog

`hslua-module-doclayout` uses [PVP Versioning][1].
The changelog is available [on GitHub][2].

## 1.2.0.1

Released 2025-06-23.

-   Relaxed upper bound for hslua, allow hslua-2.4.

## 1.2.0

Released 2024-09-21.

-   Require doclayout-0.5.

-   Add new styling functions `bold`, `italic`, `underlined`,
    `strikeout`, `fg`, and `bg`.

-   The `render` function now takes `style` as a third parameter.
    It must be either 'plain' or 'ansi' when given.

## 1.1.1.2

Released 2024-06-10.

-   Fixed another documentation bugs: `render` returns a `string`.

## 1.1.1.1

Released 2024-06-09.

-   Fixed minor typos in docstrings.

## 1.1.1

Released 2024-01-23.

-   Relaxed upper bound for text, allowing text-2.1.

-   Improved doc strings.

-   Test with latest GHC versions.

## 1.1.0

Release pending.

-   Require hslua-2.3 and tasty-lua-1.1.

-   Type info is added to all fields.

-   The `Doc` type is now associated with this module's docs,
    making it easier to generate documentation.

## 1.0.4

Released 2022-04-03.

-   Allow doclayout-0.4.

## 1.0.3

Released 2022-02-18.

-   Relaxed upper bound for hslua, allowing hslua-2.2.

## 1.0.2

Released 2022-02-05.

-   Modify functions, ensuring that the main document is always
    the first argument. This allows convenient use of these
    functions as methods.

-   Improved documentation.

## 1.0.1

Released 2022-01-31.

-   Updated to hslua-2.1.

-   Functions that take a `Doc` as the first argument are added as
    methods to `Doc` values.

## 1.0.0

Released 2021-10-24.

* Upgraded to hslua-2.0.
* Switched module name from `Foreign.Lua.Module.DocLayout` to
  `HsLua.Module.DocLayout`.

## 0.2.0.1

Released 2020-10-28.

* Relax upper bounds for hslua, allowing hslua-1.3.*

## 0.2.0

* Full test coverage of all provided Lua functions.
* Use documented module, including all function docs in the
  exported Haskell structure.
* Test with all GHC 8 major versions.

## 0.1.0

* Initial release.

[1]: https://pvp.haskell.org
[2]: https://github.com/hslua/hslua-module-doclayout/releases

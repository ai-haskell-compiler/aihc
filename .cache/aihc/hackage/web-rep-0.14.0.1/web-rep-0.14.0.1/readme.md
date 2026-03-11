
# web-rep

[![img](https://img.shields.io/hackage/v/web-rep.svg)](https://hackage.haskell.org/package/web-rep) [![img](https://github.com/tonyday567/web-rep/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/tonyday567/web-rep/actions/workflows/haskell-ci.yml)

Various functions and representations for a web page.

The best way to understand functionality is via running the example app:

    cabal install
    web-rep-example --shared

&#x2026; and then tune in to:

<http://localhost:9160/>


# library reference

-   [scotty](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html#flag-reference)
-   [bootstrap](https://getbootstrap.com/)
-   [bootstrap-slider](https://seiyria.com/bootstrap-slider)


# Development

    (setq haskell-process-args-cabal-repl '("web-rep:exe:web-rep-example"))

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left">web-rep:exe:web-rep-example</td>
</tr>
</tbody>
</table>

    :r
    :set -Wno-type-defaults
    :set -Wno-name-shadowing
    :set -XOverloadedStrings
    :set -XOverloadedLabels
    :set -XDataKinds
    import Prelude
    import Box
    import Web.Rep
    import Optics.Core
    import FlatParse.Basic
    import MarkupParse
    putStrLn "ok"

    Ok, 11 modules loaded.
    ghci
    ok


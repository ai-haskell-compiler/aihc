# Cassava Megaparsec
<!-- ALL-CONTRIBUTORS-BADGE:START - Do not remove or modify this section -->
[![All Contributors](https://img.shields.io/badge/all_contributors-8-orange.svg?style=flat-square)](#contributors-)
<!-- ALL-CONTRIBUTORS-BADGE:END -->

[![License MIT](https://img.shields.io/badge/license-MIT-brightgreen.svg)](http://opensource.org/licenses/MIT)
[![Hackage](https://img.shields.io/hackage/v/cassava-megaparsec.svg?style=flat)](https://hackage.haskell.org/package/cassava-megaparsec)
[![Stackage Nightly](http://stackage.org/package/cassava-megaparsec/badge/nightly)](http://stackage.org/nightly/package/cassava-megaparsec)
[![Stackage LTS](http://stackage.org/package/cassava-megaparsec/badge/lts)](http://stackage.org/lts/package/cassava-megaparsec)
![Build Status](https://github.com/stackbuilders/cassava-megaparsec/actions/workflows/build.yml/badge.svg)

The package provides alternative parser for the
[Cassava](https://hackage.haskell.org/package/cassava) package written with
[Megaparsec](https://hackage.haskell.org/package/megaparsec) so you can get
better error messages at expense of some speed.

## Quick start

The package works seamlessly with Cassava by replacing the following
functions:

* `decode`
* `decodeWith`
* `decodeByName`
* `decodeByNameWith`

The functions work just the same as Cassava's equivalents, but also take
name of file they parse (to include into error messages) and return typed
high-quality error messages produced by
[Megaparsec](https://hackage.haskell.org/package/megaparsec).

The import section typically looks like this:

```haskell
import Data.Csv hiding (decode, decodeWith, decodeByName, decodeByNameWith)
import Data.Csv.Parser.Megaparsec (decode, decodeWith, decodeByName, decodeByNameWith)
```

Next you call appropriate function and get either result of parsing
identical to that of Cassava or error message. The error message is
well-typed so you can examine it in Haskell code easily. Conversion error
are considered parsing errors by the `cassava-megaparsec` package and are
reported via custom error message component `Cec` supported by Megaparsec 5.
Since Cassava's conversion errors are plain strings, we have no choice but
to represent them as strings too:

```haskell
-- | Custom error component for CSV parsing. It allows typed reporting of
-- conversion errors.

data Cec
  = CecFail String
  | CecIndentation Ordering Pos Pos
  | CecConversionError String
  deriving (Eq, Data, Typeable, Ord, Read, Show)
```

To pretty print a error message use the `parseErrorPretty` function from
`Text.Megaparsec`.

This should be enough to start using the package, please consult Haddocks
for detailed per-function documentation.

## Contributors ✨

Thanks goes to these wonderful people ([emoji key](https://allcontributors.org/docs/en/emoji-key)):

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->
<table>
  <tbody>
    <tr>
      <td align="center" valign="top" width="14.28%"><a href="https://markkarpov.com/"><img src="https://avatars.githubusercontent.com/u/8165792?v=4?s=100" width="100px;" alt="Mark Karpov"/><br /><sub><b>Mark Karpov</b></sub></a><br /><a href="https://github.com/stackbuilders/cassava-megaparsec/commits?author=mrkkrp" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/cptrodolfox"><img src="https://avatars.githubusercontent.com/u/20303685?v=4?s=100" width="100px;" alt="William R. Arellano"/><br /><sub><b>William R. Arellano</b></sub></a><br /><a href="https://github.com/stackbuilders/cassava-megaparsec/commits?author=cptrodolfox" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://cristhianmotoche.github.io/"><img src="https://avatars.githubusercontent.com/u/8370088?v=4?s=100" width="100px;" alt="Cristhian Motoche"/><br /><sub><b>Cristhian Motoche</b></sub></a><br /><a href="https://github.com/stackbuilders/cassava-megaparsec/commits?author=CristhianMotoche" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/aemartinez37"><img src="https://avatars.githubusercontent.com/u/29762180?v=4?s=100" width="100px;" alt="Andrés Martínez"/><br /><sub><b>Andrés Martínez</b></sub></a><br /><a href="https://github.com/stackbuilders/cassava-megaparsec/commits?author=aemartinez37" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/Bodigrim"><img src="https://avatars.githubusercontent.com/u/2293557?v=4?s=100" width="100px;" alt="ˌbodʲɪˈɡrʲim"/><br /><sub><b>ˌbodʲɪˈɡrʲim</b></sub></a><br /><a href="https://github.com/stackbuilders/cassava-megaparsec/commits?author=Bodigrim" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://g0v.social/@felixonmars"><img src="https://avatars.githubusercontent.com/u/1006477?v=4?s=100" width="100px;" alt="Felix Yan"/><br /><sub><b>Felix Yan</b></sub></a><br /><a href="https://github.com/stackbuilders/cassava-megaparsec/commits?author=felixonmars" title="Code">💻</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/Jagl257"><img src="https://avatars.githubusercontent.com/u/27145248?v=4?s=100" width="100px;" alt="Jorge Guerra Landázuri"/><br /><sub><b>Jorge Guerra Landázuri</b></sub></a><br /><a href="https://github.com/stackbuilders/cassava-megaparsec/commits?author=Jagl257" title="Code">💻</a></td>
    </tr>
    <tr>
      <td align="center" valign="top" width="14.28%"><a href="https://www.stackbuilders.com/news/author/justin-leitgeb"><img src="https://avatars.githubusercontent.com/u/9977?v=4?s=100" width="100px;" alt="Justin S. Leitgeb"/><br /><sub><b>Justin S. Leitgeb</b></sub></a><br /><a href="https://github.com/stackbuilders/cassava-megaparsec/commits?author=jsl" title="Code">💻</a></td>
    </tr>
  </tbody>
</table>

<!-- markdownlint-restore -->
<!-- prettier-ignore-end -->

<!-- ALL-CONTRIBUTORS-LIST:END -->

This project follows the [all-contributors](https://github.com/all-contributors/all-contributors) specification. Contributions of any kind welcome!

## License

MIT, see [the LICENSE file](LICENSE).

## Contributing

Do you want to contribute to this project? Please take a look at our [contributing guideline](/docs/CONTRIBUTING.md) to know how you can help us build it.

---
<img src="https://cdn.stackbuilders.com/media/images/Sb-supports.original.png" alt="Stack Builders" width="50%"></img>  
[Check out our libraries](https://github.com/stackbuilders/) | [Join our team](https://www.stackbuilders.com/join-us/)

## 0.2.1.2

- Support megaparsec >= 9.0.0 [#25](https://github.com/egison/egison-pattern-src/issues/25)

## 0.2.1.1

- Relax version upper bound for prettyprinter and recursion-scheme.

## 0.2.1.0

- Fix use of external parsers to delimit input with `,`, `)`, or `]`
- Enable to parse value expression in the form of `#[1, 2, 3]` without parentheses

## 0.2.0.0

Breaking changes:
- Add a tuple pattern [#12](https://github.com/egison/egison-pattern-src/issues/12)
- Add a collection pattern [#17](https://github.com/egison/egison-pattern-src/issues/17)
- `UnexpectedEndOfFile` error variant is added for greedy parsers (change in [#19](https://github.com/egison/egison-pattern-src/issues/19))
- The name of parsing file is passed in `ParseMode`, not in the argument of `parseExpr` (change in [#19](https://github.com/egison/egison-pattern-src/issues/19))
- Fix syntax of constructor patterns [#18](https://github.com/egison/egison-pattern-src/issues/18)
  * We do not require constructor patterns to be parenthaized

Non-breacking changes:
- Add non-greedy parsers [#19](https://github.com/egison/egison-pattern-src/issues/19)
  * Variants of parsers are provided via `Parsable` class
- not patterns are now parsed as atom patterns (change in [#18](https://github.com/egison/egison-pattern-src/issues/18))
- `Source` type constraint is now just a type class providing few additional methods to `Stream` (change in [#19](https://github.com/egison/egison-pattern-src/issues/19))

## 0.1.1.0

Nothing changed (updated as other two adaptor packages)

## 0.1.0.0

- Initial Release

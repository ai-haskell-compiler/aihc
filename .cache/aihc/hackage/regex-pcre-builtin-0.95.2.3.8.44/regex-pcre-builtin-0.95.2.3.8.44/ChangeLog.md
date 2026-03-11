See also http://pvp.haskell.org/faq

## 0.95.1.3.8.43
- Version bump to reflect pcre version (8.43) and to have Hackage upgrade from 0.95.1.2.8.43 which does not include #15

## 0.95.1.1.8.44
- Fix: `asCString` for `Text` regexes doesn't check for null-termination, causing the compiled regex to be corrupted. (@kuribas)

## 0.95.1.0.8.43

- Update to `regex-0.94.0.0` API
- Compatibility with `base-4.13.0`
- New `Text.Regex.PCRE.Text` & `Text.Regex.PCRE.Text.Lazy` modules (merged from `regex-pcre-text` package)

----

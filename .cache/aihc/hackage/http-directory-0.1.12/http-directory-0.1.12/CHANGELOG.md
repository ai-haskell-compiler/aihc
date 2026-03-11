# Changelog

`http-directory` uses [PVP Versioning](https://pvp.haskell.org).

## 0.1.12 (2025-11-20)
- send HTTP directory requests with `"Accept: text/html"`
- send HTTP HEAD requests with `"Accept: */*"`

## 0.1.11 (2025-02-08)
- add httpFileSizeAndTime'
- tests: switch from httpbingo to httpbin.org/redirect
- redefine +/+ with dropWhile/End to avoid head/init/tail/last
- add simple cli tool
- remove support for ghc < 8.2 (base < 4.9)

## 0.1.10 (2022-06-11)
- add httpFileSizeTime and httpFileHeaders

## 0.1.9 (2021-12-09)
- httpDirectory' now uses Network.HTTP.Simple
- also add httpRawDirectory', httpExists', httpFileSize', httpLastModified'
  which all use Network.HTTP.Simple too
- add +/+ path combinator

## 0.1.8 (2020-03-12)
- fix regression in 0.1.6 and 0.1.7: do not filter "*/" files
- more careful filter handling of '/'
- filter infix '#'

## 0.1.7 (2020-01-24)
- drop </> since it conflicts with filepath
- deprecates 0.1.6

## 0.1.6 (2020-01-24)
- filter relative/paths and relative?queries
- add `url </> file`
- add trailingSlash for url and noTrailingSlash for filename

## 0.1.5 (2019-06-xx)
- also filter "..", "#" and ":" from httpDirectory
- export Manager
- add isHttpUrl
- testsuite added

## 0.1.4 (2019-06-07)
- add httpRawDirectory
- httpDirectory now filters out absolutes hrefs and sort links
- add httpDirectory' and httpRedirect' variants with own Manager

## 0.1.3 (2019-06-04)
- add httpExists

## 0.1.2 (2019-05-07)
- add httpManager convenient function
- print url when result not 200
- add an example

## 0.1.1 (2019-04-14)
- add httpLastModified

## 0.1.0 (2019-04-11)
- initial release with httpDirectory, httpFileSize, and httpRedirect(s)

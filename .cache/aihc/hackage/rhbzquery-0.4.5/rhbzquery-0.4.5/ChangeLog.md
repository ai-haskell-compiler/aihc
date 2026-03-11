# rhbzquery releases

## 0.4.5 (2025-01-20)
 Fields: quote unknown field
 ParseArg parseField: error on empty parameter
 User: use simple-prompt promptNonEmpty
 Help: use ansi-wl-pprint for compatibility with recent optparse-applicative

## 0.4.4 (2021-09-06)
- experimental '--reverse' command: converts url to args
- add 'arch'/'platform' aliases
- '--list-fields' now prints field aliases too

## 0.4.3 (2021-02-15)
- support status with '<STATE' and '>STATE'
- Bugzilla uses 'notequals' (not 'notequal')
- fix missing EPEL version prefix

## 0.4.2 (2021-01-08)
- add --query command
- add --server option

## 0.4.1 (2020-12-17)
- set no stdout buffering (reported by @clrkwllms)
- change license from BSD3 to GPLv2+ and add SPDX tags

## 0.4 (2020-12-11)
- add --list-operators
- rename longer operators to be more memorable

## 0.3 (2020-12-10)
- add --list-fields option
- field validation: error for unknown fields
- for 'content' field, map '~'/'!~' to matches/notmatches
- refactor operator handling to generate help

## 0.2 (2020-12-09)
- support most search type operators (regexp, substr, isempty, and negatives)

## 0.1.1 (2020-12-04)
- --file a bug
- --help: do not format FIELDS
- add testsuite and travis

## 0.1.0 (2020-12-02)
- initial release: supports field parameters, status, flags
- has --mine
- opens urls with xdg-open

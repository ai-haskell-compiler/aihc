# Release history for lsfrom

## 2.0 (2025-02-10)
- major rewrite introducing --from file option
- --after, --until, --before are also file options
- uses locale collation with C FFI
- add --dirs and --files file-type filters

## 1.0 (2024-01-07)
- add --after: start with the next file after the specified one
- add --until: include files only up to the specified file
- add --before: changes the behaviour of --until
- add --strict: error if the specified files do not exist
- the short option for --all is now -A

## 0.1.1.1 (2022-04-24)
- add missing test files
- fix testsuite for lts 12 and earlier

## 0.1.1 (2022-04-24)
- use `ls -A`
- improve documentation

## 0.1 (2021-02-06)
- initial Hackage release
- supports filepaths now not just file pattern
- uses system locale collation

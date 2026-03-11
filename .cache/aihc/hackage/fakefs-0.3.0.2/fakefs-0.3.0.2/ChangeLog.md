# Changelog for fakefs

## 0.3.0.2 (2020-09-05)

- :bug: Support GHC 8.6 or later.
- :memo: Update the example in README to follow the changes in 0.3.0.0.

## 0.3.0.1 (2020-08-11)

- :memo: Update README to follow the changes in 0.3.0.0.

## 0.3.0.0 (2020-08-11)

- Redesign: Add ability to throw `IOException`s in a more composable manner.
    - By deleting `ExceptT` from `FileSystemT`, it's more `GeneralizedNewtypeDeriving`-friendly.
    - Generalize the monad of `readPath` and `writePath` etc. by the new constraint synonym `MonadFileSystem`.

## 0.2.0.1 (2019-07-16)

- :memo: Update READE with an example.
- :memo: Correct wrong repository URL in the cabal file.

## 0.2.0.0 (2019-03-30)

- Simplify the monad stack and the file object.
- Rename main APIs to express its generality.
    - `readFileT` -> `readPath`
    - `writeFileT` -> `writePath`
- Add utility functions: `evalFileSystemT`, `execFileSystemT`, etc.

## 0.0.1.0 (2019-03-29)

Initial Release

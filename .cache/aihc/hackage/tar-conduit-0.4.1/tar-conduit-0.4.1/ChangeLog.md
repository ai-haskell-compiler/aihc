# ChangeLog for tar-conduit

## 0.4.1 - 2024-01-06

* Fix bug in the parsing of pax extended header data when provided in more than
  one sequential chunk [#37](https://github.com/snoyberg/tar-conduit/issues/37)

## 0.4.0 - 2023-08-07

* `untarChunks` and `untar` now provide partial support for the pax interchange
  format, by applying pax header blocks and certain keywords in pax extended
  headers. Exposes `applyPaxChunkHeaders`. Also exposes `untarChunksRaw` and
  `untarRaw`, which do not apply pax header blocks and extended headers.

## 0.3.2.1 - 2023-06-25

* `unix-2.8` compatibility [#32](https://github.com/snoyberg/tar-conduit/pull/32)

## 0.3.2 - 2019-01-14

* Create a prefix directory whenever a directory type entry is missing from the tarball itself. Fixes [#26](https://github.com/snoyberg/tar-conduit/issues/26).

## 0.3.1 - 2018-11-19
 * Fixed modification time restoration on Windows for read-only files [#25](https://github.com/snoyberg/tar-conduit/pull/25).

## 0.3.0 - 2018-08-28
 * Support for `FTHardLink` restoration, but not creation yet.
 * Restoring files made even more lenient with creation of full directory path if any parents of
   it are missing.

## 0.2.5 - 2018-08-28
 * Exported `untarWithExceptions`, `restoreFileIntoLenient`

## 0.2.4 - 2018-08-27
 * Use forward slashes on Windows too, see [issue #21](https://github.com/snoyberg/tar-conduit/issues/21)
 * Addition of `extractTarballLenient` and `restoreFileWithErrors`

## 0.2.3.1 - 2018-06-06
 * Fixed drops associated payload for unsupported headers ((https://github.com/snoyberg/tar-conduit/issues/17)) ([PR 18](https://github.com/snoyberg/tar-conduit/pull/18))
 * Dropped support for GHC 7/Stack LTS-2, LTS-3, LTS-6

## 0.2.3 - 2018-02-10

 * Fixed compatibility with new `conduit >= 1.3.0`

## 0.2.2 - 2018-02-06

 * Fixed proper unicode filepaths handling.
 * Fixed restoration of symbolic links.
 * Fixed restoring files with long names (>255), that use GNU tar format.
 * Utilizing GNU tar standard implemented support for long (>2097151) values of OwnerId, GroupId,
   DeviceMinor and DeviceMajor values, as well as (>8589934591) for FileSize and
   ModificationTime. Thus removing the 8GB size limitation, allowing negative timestamps and fixing
   compatibility with systems that use UID and GID in the higher range.
 * Expose `withFileInfo`.
 * Improved error reporting.

## 0.2.1 - 2018-02-03

 * Expose `untarChunks`

## 0.2.0 - 2018-01-23

 * Implemented tarball creation.
 * Introduced `FileInfo` datatype that makes it easier to work with archives by automatically
   handling tar specific blocks and applying them to `FileInfo`.
 * Full support for `ustar` format.
 * Support for GNU tar features: long file name. Discardes unsupported.
 * Helper tar/untar functions for dealing directly with the filesystem.

## 0.1.1 - 2017-05-31

 * Allow checksums to have leading spaces ([PR 8](https://github.com/snoyberg/tar-conduit/pull/8))

## 0.1.0 - 2016-11-03

 * Initial release

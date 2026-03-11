# Revision history for reflex-fsnotify

## 0.3.0.2
* Support GHC 9.12

## 0.3.0.1
* Build with GHC 9.10

## 0.3.0.0

* Require fsnotify >= 0.4, which has a breaking API change
* Support reflex-0.9

## 0.2.1.3

Support GHC 8.10

## 0.2.1.2

* Support reflex 0.8

## 0.2.1.1

* Support GHC 8.8 and `reflex` 0.7.

## 0.2.1.0

* Add `watchDirs` to watch a list of directories
* Generalize `wrapWatch` a little to make `watchDirs` possible

## 0.2.0.0

* Deprecate `watchDirectory`
* Add `watchDir` and `watchTree` corresponding to the functions in `System.FSNotify`
* Add `watchDirectoryTree`, an alternative to `watchTree` that tries to break symlink cycles

## 0.1.0.0

* Initial release containing `watchDirectory`.

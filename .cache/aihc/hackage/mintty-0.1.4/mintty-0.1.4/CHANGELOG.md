### 0.1.4 [2022.06.30]
* Remove an unused dependency on `filepath`.

### 0.1.3 [2021.11.07]
* Backport a fix for a `Win32` bug that would make `isMinTTY` incorrectly
  return `False` on recent versions of MinTTY.

### 0.1.2 [2018.05.07]
* Only use the `Win32`-provided version of `isMinTTY` if building against
  `Win32-2.5.3` to be certain that one avoids Trac #13431.
* Don't enable `Safe` on GHC 7.2.

### 0.1.1 [2017.03.17]
* Work around a serious bug on 32-bit Windows GHC that causes linker errors
  when mintty is used together with code that uses certain `msvcrt` functions,
  such as `atan`

## 0.1 [2017.01.30]
* Initial release

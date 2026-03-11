## 0.4.4 [2026.01.10]
* Allow building with `base-4.22.*` and `time-1.15.*` (GHC 9.14).
* Allow building with `scotty-0.30.*`.
* Remove unused `transformers` and `unordered-containers` dependencies.

## 0.4.3 [2024.10.26]
* Allow building with `data-default-class-0.2.*`.
* Drop support for pre-8.0 versions of GHC.

## 0.4.2 [2023.10.05]
* Support building with `scotty-0.20`.

## 0.4.1 [2021.10.09]
* Allow building with `aeson-2.0.0.0`.

## 0.4
* Require `scotty` >= 0.10
* `connect` now returns `IO (ScottyM ())`
* Allowed building with `base-4.8.0.0` and older `base`s (down to 4.3)
* Added `Eq` instances for `Document` and `Options`, and `Ord` and `Show` instances for `Options`

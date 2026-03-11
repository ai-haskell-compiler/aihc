# 0.1.1 -- 2023-03-10

* Backport `Eq` and `Ord` instances for `Generically1`, which were introduced
  in `base-4.18.0.0`. To ensure that these instances are in scope when building
  with `base-4.17.0.0` (in which `Generically1` is defined in `base`, not this
  library), we also add a dependency on `base-orphans`.

# 0.1 -- 2022-06-15

* First version.

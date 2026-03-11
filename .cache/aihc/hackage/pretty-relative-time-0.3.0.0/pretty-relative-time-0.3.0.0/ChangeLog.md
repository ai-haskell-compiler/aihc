# Changelog for pretty-relative-time

## [0.3.0.0] - 2021-11-20

### Changed

* Support for `genvalidity` >=1.0.0.0

## [0.2.0.0] - 2020-02-12

### Changed

Fixed a bug like this:

```
 approximateDaysPerMonth :: Integral a => a
-approximateDaysPerMonth = 30
+approximateDaysPerMonth = 31
```


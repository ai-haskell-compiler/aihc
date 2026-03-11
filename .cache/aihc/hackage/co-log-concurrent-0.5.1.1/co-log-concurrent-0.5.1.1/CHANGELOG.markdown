# Revision history for co-log-concurent

## 0.5.0.1 -- 2022-08-03
  - Bumped co-log-core version to ^>= 0.3.
  - Bumped stack resolver to lts-18.19 (GHC 8.10.7)

## 0.5.0.0 --

* API changes:

  - Introduce mkCapacity function, as previously it was not possible
    to create custom capacity.
  - Fork background logger now takes an explicit flush action. And
    reads logs in chunks.

## 0.4.0.0 -- 2020-04-18

* Library extracted from co-log.

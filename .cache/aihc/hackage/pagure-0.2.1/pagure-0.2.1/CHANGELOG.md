# Changelog

`pagure-hs` uses [PVP Versioning](https://pvp.haskell.org).

## 0.2.1 (2024-05-06)
- add pagureGroupRepos
- export getRepos helper

## 0.2.0 (2024-05-06)
- queryPagureCountPaged replaces queryPagurePaged
- warnings now go to stderr
- remove queryPagure' which errored
- queryPagureCount and queryPagurePaged: no longer error

## 0.1.2 (2024-05-05)
- add pagureGroupInfo for group endpoint

## 0.1.1 (2022-02-17)
- add queryPagure' which errors when JSON response has "error" field
  and use it for queryPagureCount and queryPagurePaged
  This requires http-query-0.1.2 or later to avoid early exceptions
  and get meanful error messages instead

## 0.1.0 (2021-12-27)
- Initial Hackage release

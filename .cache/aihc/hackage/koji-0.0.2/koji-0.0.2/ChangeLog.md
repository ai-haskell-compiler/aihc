# Changelog for koji-hs

`koji` uses [PVP Versioning](https://pvp.haskell.org).

## 0.0.2 (2021-05-07)
- listBuilds now supports 'pattern' parameter available in Koji-1.24,
  which can be used to glob match the nvr
  (the rest of the 1.24 changes don't affect the current koji-hs API)

## 0.0.1 (2021-04-15)
- First Hackage release with higher level Distribution.Koji and
  low level Distribution.Koji.API modules. Only query procedures.

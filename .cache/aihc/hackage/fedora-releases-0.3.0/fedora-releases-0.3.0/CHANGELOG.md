# Changelog

`fedora-releases` uses [PVP Versioning](https://pvp.haskell.org).

## 0.3.0 (2025-06-04)
- Branch: add epel10.x minor branches
- Release: include eol field

## 0.2.1 (2025-02-07)
- Release: add getRawhideVersion and getCurrentFedoraVersion
- eitherBranch: fix misparse (read error) of "epel/" as branch

## 0.2.0 (2024-12-04)
- newerBranch now returns Maybe Branch
- custom Ord Branch instance to correct the order of EPELNext
- export extended Distribution.Fedora.BodhiReleases
- getActiveReleases replaces getReleases (no longer reversed)
- getActiveBranches (reverse sorted) replaces getFedoraBranches
- similarly getActiveBranched replaces getFedoraBranched

## 0.1.0 (2024-08-16)
- project renamed from [fedora-dists](https://hackage.haskell.org/package/fedora-dists) to fedora-releases
- uses Bodhi releases API endpoint instead of obsolete PDC Product versions
- Distribution.Fedora and the Dist type in particular are gone

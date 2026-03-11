# cabal-file

cabal-file is a library on top of the hackage-security library for accessing
cabal files and versions, and also a small tool for getting and comparing
.cabal files.

Since it accesses package and .cabal file data directly from
the local Hackage repo cache index tar archive under ~/.cabal/ it is quite fast.

## Usage
List all Hackage packages:
```
$ cblfile list | grep ghcide
ghcide
```

List versions of a package:
```
$ cblfile list pandoc
0.4
:
:
2.9.2.1
```

Diff .cabal files of package versions
```
$ cblfile diff pandoc 2.8 2.9.2.1
--- pandoc-2.8.cabal    2020-05-24 19:47:58.487921450 +0800
+++ pandoc-2.9.2.1.cabal        2020-05-24 19:47:58.487921450 +0800
@@ -1,10 +1,10 @@
 name:            pandoc
-version:         2.8
+version:         2.9.2.1
 cabal-version:   2.0
:
:
```

Latest version of a package:
```
$ cblfile latest extra
1.7.1
```

Timestamp for the latest .cabal file (or for a specific version):
```
$ cblfile date dhall
2020-05-19 02:18:01 UTC
```

Save a .cabal file (latest or specific version)
```
$ cblfile get purescript
$ ls purescript.cabal
purescript.cabal
```

Check package source metadata:
```
$ cblfile metadata tls-1.5.4
Size: 150171
SHA256 Hash "ce42bfa581a50f62776581da4b86408ebb1a51bc6cb2f95ad0a2ac7caa19a437"
```

## Depends output
`cblfile depends` outputs quite verbose package dependency lists.

## Library
For library documentation see the [Hackage.Index](https://hackage.haskell.org/package/cabal-file/docs/Hackage-Index.html) documentation.

For example usage see [`app/Cmds.hs`](https://hackage.haskell.org/package/cabal-file/src/app/Cmds.hs).

## Motivation
I originally wrote the `depends` command for displaying dependency information
about packages in a more friendly way.

Later, since [hdiff](http://hdiff.luite.com/) is out of sync for some packages,
and I use it particularly for comparing versions of .cabal files,
I decided to create the `diff` command.
Realising I could pull .cabal files direct from the cabal index cache,
the rest of cabal-file flowed on from there.

## Related
If you want full diffs of Hackage sources you can try out cabal-diff from
[cabal-extras](https://github.com/phadej/cabal-extras).

odebuild command
----------------

This package includes on-the-fly build command 'odebuild'.

```
Usage: odebuild clean
       odebuild prepare [options] [-- [cabal-debian-options]]
       odebuild source [options] [-- [cabal-debian-options [-- [debuild-options]]]
       odebuild build [options] [-- [cabal-debian-options [-- [debuild-options]]]
       odebuild install [options] [-- [cabal-debian-options [-- [debuild-options]]]
       odebuild reinstall [options] [-- [cabal-debian-options [-- [debuild-options]]]
         reinstall (Remove and install) support only for Haskell library packages

      --revision=DEBIAN_REVISION      debian package revision to pass to cabal-debian
                                      revision string may use on auto-generating debian directory
      --install-deps                  install build depends before running build
      --mode={All|Bin|Src|Dep|Indep}  add build-mode to build-mode list to specify
  -R  --reuse-source                  not clean generated source directory, and build operation will reuse it
```

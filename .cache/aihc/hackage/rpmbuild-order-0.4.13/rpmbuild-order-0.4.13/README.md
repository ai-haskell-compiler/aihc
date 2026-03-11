[![GitHub CI](https://github.com/juhp/rpmbuild-order/workflows/build/badge.svg)](https://github.com/juhp/rpmbuild-order/actions)
[![Hackage](http://img.shields.io/hackage/v/rpmbuild-order.png)](http://hackage.haskell.org/package/rpmbuild-order)
[![license](https://img.shields.io/badge/license-BSD-brightgreen.svg)](https://opensource.org/licenses/BSD-3-Clause)

# rpmbuild-order

This is a tool to sort RPM source packages in build dependency order.
The code was originally derived from
[cabal-sort](http://hackage.haskell.org/package/cabal-sort)
by Henning Thielemann.

## Usage

`$ rpmbuild-order --version`

```
0.4.13
```
`$ rpmbuild-order --help`

```
Order packages by build dependencies

Usage: rpmbuild-order [--version] COMMAND

  Sort package sources (spec files) in build dependency order

Available options:
  -h,--help                Show this help text
  --version                Show version

Available commands:
  sort                     sort packages in build order
  deps                     sort dependencies in neighbouring package dirs
  rdeps                    sort dependents in neighbouring package dirs
  layers                   ordered output of dependency layers
  chain                    ordered output suitable for a chain-build
  leaves                   List of the top leaves of package graph
  roots                    List lowest root packages
  render                   Show graph with graphviz
```

```
$ rpmbuild-order sort mycore mylib myapp
mylib mycore myapp
```

The arguments passed can either be directories containing the package
or spec files.

If the dependency graph has cycles then an error will be output with
a list of cycles and any shortest path subcycles.

Using the rpmbuild-order `deps` and `rdeps` commands the ordered
dependencies and reverse dependencies of a package can be obtained
within the current set of checked out package sources.
ie If you have a directory with packages:
```
pkg1/ pkg2/ lib1/ lib2/ lib3/ misc1/
```
then the output of `rpmbuild-order deps pkg1` might be `lib1 lib3 pkg1`
for example.

The `render` command displays a graph of package dependencies
using graphviz and X11 or can print the dot format to stdout.

## Library
As of version 0.4, a library is also provided.

There are two modules:

- `Distribution.RPM.Build.Order` provides higher level functions for
  sorting packages in build dependency orders and output. It is built on top of:
- `Distribution.RPM.Build.Graph` provides lower level functions for generating
  RPM dependency graphs

Please see their documentation for more details.

## Notes and known problems
0. Handles pkgconfig() provides by grepping .spec for .pc files

1. Given packages A, B, C, where C depends on B, and B depends on A,
and you call

       rpmbuild-order sort C.spec A.spec

   then the output may be wrong if C does not have a direct dependency on A.
   Even if the order is correct, B is missing in the output
   and thus in this case the list of packages cannot be reliably used
   for a sequence of builds.

   However the `deps` and `rdeps` commands take
   other neighbouring package directories into account.

2. repoquery is not used to resolve meta-dependencies or files to packages.
So if a package BuildRequires a file, it will not be resolved to a package.
This may get addressed some day, but file dependencies seem uncommon for
BuildRequires compared to Requires.

3. rpmspec is used to parse spec files (for macro expansion etc):
so missing macros packages can lead to erroneous results in some cases.

4. Since version 0.4.6 there is some support now for packages using
dynamic buildrequires (in Fedora: golang, python, ruby, and rust packages).

5. Since version 0.4.8 %{_isa} suffixed Provides are filtered out for x86_64.

## Installation

rpmbuild-order is packaged in Fedora Linux.

## Building from source

Use `cabal-rpm builddep && cabal install` or `stack install`.

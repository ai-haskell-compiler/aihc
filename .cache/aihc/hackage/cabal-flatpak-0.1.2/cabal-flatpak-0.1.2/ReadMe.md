## Scope

This program generates a large Flatpak manifest in JSON format
with the versions of all transitively imported Haskell packages.
This is required by Flathub
since this platform downloads all source files for you
and lets you build only in a sandbox without access to the internet.
The detailed version information can also be useful for you
for reproducible builds.
However, for maintaining your own Flatpak repository
or for distributing Flatpak bundles
it is not strictly necessary to have such a verbose manifest.
You may get results quicker by writing your own manifest
which simply runs `cabal-install` or `stack`.
`stack` should give you reproducible builds
but its package database contains only a subset of Hackage.
`cabal-install` gives you access to more Haskell packages,
but the build may fail at a later point due to lax version bounds
in dependent packages.


## Usage

### Create a Flatpak manifest

The usage is a bit cumbersome,
because for Flathub you may only build manifests
for released versions of your package,
whereas you certainly want to manage manifests
in the working copy of your repository.

First let `cabal` build a `plan.json` for your released package:

~~~~
$ cd /tmp
$ cabal unpack my-package-0.1.2
$ cd my-package-0.1.2
$ cabal new-build --dry-run --disable-tests --disable-benchmarks
~~~~

`cabal-flatpak` reads `plan.json` via the `cabal-plan` library.

Now create a `flatpak.cabal.json` file in your working copy.
It contains a custom JSON object with information needed by `cabal-flatpak`
and also a template for the generated manifest.
For an example see the configuration file for `cabal-flatpak`.

~~~~
$ cd /path/to/my-package
$ cabal-flatpak --directory=/tmp/my-package-0.1.2 flatpak.cabal.json flatpak.json
~~~~

The `--arch` option allows you to put build information
for one or more architectures into one manifest.
Usually, only `ghc` and `cabal-install` binaries depend on the architecture
and the whole lot of Haskell packages can be build with the same commands.

There are two build modes:
One builds all modules individually using plain `Cabal`,
the other one builds all modules in one go using `cabal-install`.
You can enable the second mode using the `--cabal-install` option.
These are the differences:

* `Cabal` mode needs less dependencies.
  `cabal-install` needs a pre-built binary for your architecture.

* `Cabal` mode can pass options to the build of specific packages.
  `cabal-install` can only pass options to all packages at once.
  If `-fbuildExamples` means something different to different Haskell packages,
  then this will fail.
  Even in `cabal-install` mode we need to preprocess each package
  and in this stage we could alter flag switches by patching Cabal files.
  We could also build packages with requested flags in separate stages.
  Currently, we don't try any of these strategies.

* `cabal-install` enables parallel builds.
  This builds significantly faster.
  `Cabal` mode on the other hand allows Flatpak to cache build results
  of individual packages.
  This can accelerate re-builds.
  However, Flatpak does not know the dependency graph
  and thus simply rebuilds anything after a module that must be rebuilt.

Known issue:
If you use the `"main-sources"` list of manually written sources,
then the main tarball needs `"type": "archive"` for `Cabal` mode
and `"type": "file"` for `cabal-install`.
That is you cannot simply switch between both modes only at the command-line.
I suggest to not use `"main-sources"` anyway,
but stick to versions published at Hackage.

For some packages you need to build dependencies on external C packages.
`cabal-flatpak` cannot generate according build instructions for you.
However, you can re-use build instructions you found useful.
Flatpak-builder supports this itself.
In the `"modules"` list you cannot only put module JSON objects,
but also plain strings.
Such a string is interpreted as path to a separate file
containing a Flatpak module JSON object.
I add such JSON files to the FFI packages I maintain.


### Build the Flatpak package

First you need to install a Flatpak runtime environment and its SDK:

~~~~
$ sudo flatpak install org.freedesktop.Platform org.freedesktop.Sdk org.freedesktop.Platform.Compat.i386 org.freedesktop.Sdk.Compat.i386
~~~~


You may refer to the `Makefile` that is shipped with `cabal-flatpak`
for how to eventually build the Flatpak package.
The command line is:

~~~~
$ flatpak-builder --force-clean --repo=$FLATPAK/repository --state-dir=$FLATPAK/builder/ $FLATPAK/build/my-package $<
~~~~

Flatpak consumes pretty much storage
thus I set $FLATPAK to a directory on a separate harddisk partition.
The `--repo` option points to your Flatpak repository.
This is where `flatpak-builder` stores the compiled package.
The Flatpak repository contains all versions of all your Flatpak packages.
If another user has access to it,
she can easily install and update Flatpak packages.
The `--state-dir` option points to a directory
that caches all downloads and build artifacts of Flatpak.
You may share it between different projects.
The `DIRECTORY` argument names the path
to where Flatpak builds your project.
It may not be shared between projects,
but you can safely delete it after a build
and the `--force-clean` option triggers exactly this
when you re-build your project.
The directories specified by `--state-dir` and `DIRECTORY`
must reside on the same file system.

You may also extract a single package file
for a certain version of your package from the repository.
This can be handy for a one-time install
but disallows the user to easily get updates of your program.

~~~~
$ flatpak build-bundle $FLATPAK/repository \
    my-package-0.1.2.flatpak com.my_domain.my-package \
    --runtime-repo=https://flathub.org/repo/flathub.flatpakrepo
~~~~


## Known issues

### Cabal revisions

Cabal package description files can be updated on Hackage
without altering the package version.
Every update increases the Cabal file revision.
Unfortunately, `plan.json` does not contain the revision number,
it just refers to the currently most recent Cabal file for each Haskell package.
This is a fundamental problem since Flatpak builds must be reproducible.
We currently solve this problem
by scanning Cabal's local package database
below `.cabal/packages/hackage.haskell.org`.
This has not optimal performance but it is acceptable.

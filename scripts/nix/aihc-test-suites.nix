# Build and run the test suites of the project packages with aihc, not
# with GHC. This is the first step to replace GHC in the checks.
#
# Two derivations do the work:
#
# * The Hackage sources: a fixed-output derivation that runs
#   `aihc plan PACKAGE --locked` for each package. Planning downloads each
#   release of the plan, and a locked plan reads only answers that do not
#   change: the release archives and the cabal files at their locked
#   revisions. The output is the part of the aihc cache that holds them.
# * The test suites: `aihc test PACKAGE --locked --offline` for each
#   package, with a copy of that cache and no network. All packages share
#   one store, so each dependency compiles one time.
#
# The aihc.lock file of each package gives its plan for each platform. After
# a change to a lock, set the hash of each system to the empty string,
# build the sources, and copy the hash that Nix reports.
{
  pkgs,
  # The aihc executable that builds the test suites.
  aihc,
  # cabal.project, the cabal files, and the aihc.lock files.
  planSource,
  # The sources of the project packages and their test fixtures.
  testSource,
  # Shell text that sets the environment of the test suites.
  testSetup,
}: let
  inherit (pkgs.stdenv.hostPlatform) system;

  # The aihc target of each system that has a plan in the lock files.
  targets = {
    aarch64-darwin = "apple-arm64";
    x86_64-linux = "linux-amd64";
  };

  # The hash of the Hackage sources of the plans of each system.
  sourcesHashes = {
    aarch64-darwin = "sha256-mWdeEswiwvY1Jj8JEUs9yj6JuSdXIFoEgdPxNeuKU+I=";
    x86_64-linux = "sha256-91uDnKQRA91hcNvLMZTPLxuBHBjIyJfJxbfy/ycJWg8=";
  };

  # The project packages that have a test suite and a plan with Hackage
  # packages, as their aihc.lock files give it.
  packages = [
    "aihc-resolve"
    "aihc-tc"
    "aihc-testing"
    "aihc-hackage"
    "aihc-hackage-fetch"
    "aihc-package-plan"
    "aihc-haddock"
    "aihc-play"
    "aihc"
  ];

  target = targets.${system} or null;

  # The test threads of a package, when they are not 4. The aihc-play suite
  # sometimes stops with a segmentation fault when its tests run on more
  # than one thread (about one run in five on aarch64-darwin). With one
  # thread it does not. This is a defect of aihc, which this setting does
  # not correct.
  testThreads = {
    aihc-play = 1;
  };

  # The C libraries that the packages of the plans link: libffi for
  # aihc-testing, zlib for zlib, and curses for terminfo.
  systemLibraries = [pkgs.libffi pkgs.zlib pkgs.ncurses];

  hackageSources =
    pkgs.runCommand "aihc-test-suite-hackage-sources" {
      nativeBuildInputs = [aihc pkgs.cacert];
      outputHashMode = "recursive";
      outputHashAlgo = "sha256";
      outputHash = sourcesHashes.${system};
    } ''
      export HOME="$TMPDIR"
      export XDG_CACHE_HOME="$TMPDIR/cache"
      export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
      export SYSTEM_CERTIFICATE_PATH=${pkgs.cacert}/etc/ssl/certs
      cp -R --no-preserve=mode ${planSource} "$TMPDIR/source"
      cd "$TMPDIR/source"
      export AIHC_CORE_LIBS_ROOT="$PWD"
      ${pkgs.lib.concatMapStrings (package: ''
          echo "Getting the Hackage sources of ${package}"
          aihc plan ${package} --locked --target ${target} > /dev/null
        '')
        packages}
      # Only the release archives and the cabal files at a locked revision.
      # The version lists, the newest revisions, and the logs can change.
      mkdir -p "$out/hackage-query/cabal"
      cp -R "$XDG_CACHE_HOME/aihc/hackage" "$out/hackage"
      find "$XDG_CACHE_HOME/aihc/hackage-query/cabal" -name '*-r[0-9]*.cabal' -exec cp {} "$out/hackage-query/cabal/" \;
    '';

  testSuites =
    pkgs.runCommandCC "aihc-test-suites" {
      # Some Hackage dependencies have Happy or Alex sources, as the grammar
      # of pretty-show, a dependency of hedgehog.
      nativeBuildInputs = [aihc pkgs.coreutils pkgs.haskellPackages.happy pkgs.haskellPackages.alex];
    } ''
      ${testSetup}
      export HOME="$TMPDIR"
      export XDG_CACHE_HOME="$TMPDIR/cache"
      mkdir -p "$XDG_CACHE_HOME/aihc"
      cp -R --no-preserve=mode ${hackageSources}/. "$XDG_CACHE_HOME/aihc/"
      cp -R --no-preserve=mode ${testSource} "$TMPDIR/source"
      cd "$TMPDIR/source"
      # A test suite that aihc builds finds the data files of its package
      # with this variable.
      export aihc_datadir="$PWD/bin/aihc"
      # The C libraries of the packages.
      export CPATH="${pkgs.lib.makeSearchPathOutput "dev" "include" systemLibraries}''${CPATH:+:$CPATH}"
      export LIBRARY_PATH="${pkgs.lib.makeLibraryPath systemLibraries}''${LIBRARY_PATH:+:$LIBRARY_PATH}"
      ${pkgs.lib.concatMapStrings (package: ''
          echo "Testing ${package}"
          time timeout 1800 aihc test ${package} --locked --offline \
            --store "$TMPDIR/store" --build-root "$TMPDIR/build" -O0 \
            --test-option=--hide-successes --test-option=--hide-progress \
            --test-option=--num-threads --test-option=${toString (testThreads.${package} or 4)}
        '')
        packages}
      touch "$out"
    '';
in
  pkgs.lib.optionalAttrs (target != null) {
    aihc-test-suite-hackage-sources = hackageSources;
    aihc-test-suites = testSuites;
  }

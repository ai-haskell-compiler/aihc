{
  projectHsPackages,
  mkHsPkgs,
  mkWasiSysroot,
}: pkgs: let
  hsPkgs = mkHsPkgs pkgs;
  playTools = import ./play-tools.nix {inherit pkgs;};
  wasmSysroot = mkWasiSysroot pkgs;
  smokeBrowsers = pkgs.playwright-driver.browsers.override {
    withFirefox = false;
    withWebkit = false;
    withFfmpeg = false;
    withChromiumHeadlessShell = false;
  };
  wasmLd = pkgs.writeShellScriptBin "wasm-ld" ''
    exec ${pkgs.lld}/bin/wasm-ld "$@"
  '';
  resolveProgressExe = pkgs.lib.getExe' hsPkgs.aihc-resolve-tooling-common "resolve-progress";
  resolveExtensionProgressExe = pkgs.lib.getExe' hsPkgs.aihc-resolve-tooling-common "resolve-extension-progress";
  fixtureExtensionProgressExe = pkgs.lib.getExe' hsPkgs.aihc-testing "fixture-extension-progress";
  fixtureProgressExe = pkgs.lib.getExe' hsPkgs.aihc-testing "fixture-progress";
  tcProgressExe = pkgs.lib.getExe' hsPkgs.aihc-tc-tooling-common "tc-progress";
  aihcDevExe = pkgs.lib.getExe' hsPkgs.aihc "aihc-dev";
  aihcExe = pkgs.lib.getExe' hsPkgs.aihc "aihc";
  unicode = import ./unicode.nix {inherit pkgs;};
  # The C libraries that packages of docs/self-hosting-packages.md bind to:
  # libffi for aihc-testing, ncurses for terminfo, and zlib for zlib. Clang reads
  # CPATH for headers and LIBRARY_PATH for libraries. aihc runs pkg-config
  # for pkgconfig-depends, and pkg-config reads PKG_CONFIG_PATH.
  selfHostingCLibraries = [pkgs.libffi pkgs.ncurses pkgs.zlib];

  # The PKG_CONFIG_PATH of some C libraries. A library puts its .pc file in
  # lib/pkgconfig or in share/pkgconfig. zlib uses share/pkgconfig.
  pkgConfigPath = libraries:
    pkgs.lib.concatStringsSep ":" [
      (pkgs.lib.makeSearchPathOutput "dev" "lib/pkgconfig" libraries)
      (pkgs.lib.makeSearchPathOutput "dev" "share/pkgconfig" libraries)
    ];

  # The C libraries of a self-compiled aihc executable. They are the same as
  # selfHostingCLibraries, but staticZlib replaces the shared zlib.
  selfCompileCLibraries = [pkgs.libffi pkgs.ncurses];

  # The aihc executable links zlib statically. A shared zlib from the Nix
  # store would make the release executable fail on a computer without that
  # store path. The macOS SDK of Nix has no zlib that a program can link.
  # On macOS, the library is for macOS 11, the oldest version of the Apple
  # target.
  staticZlib =
    if pkgs.stdenv.hostPlatform.isDarwin
    then
      pkgs.zlib.overrideAttrs (old: {
        buildInputs = (old.buildInputs or []) ++ [(pkgs.darwinMinVersionHook "11.0")];
      })
    else pkgs.zlib;

  # The C libraries of the Hackage survey. Many Hackage packages bind to one
  # of these libraries. Without the library, GHC cannot compile the package.
  surveyCLibraries =
    selfHostingCLibraries
    ++ [
      pkgs.blas
      pkgs.bzip2
      pkgs.expat
      pkgs.file
      pkgs.gsl
      pkgs.icu
      pkgs.lapack
      pkgs.libmysqlclient
      pkgs.libpq
      pkgs.libusb1
      pkgs.libxml2
      pkgs.openssl
      pkgs.pcre
      pkgs.rdkafka
      pkgs.secp256k1
      pkgs.sqlite
      pkgs.unixodbc
      pkgs.xz
      pkgs.zeromq
    ];
  repoRootGuard = ''
    test -f cabal.project || {
      echo "Run this app from the repository root." >&2
      exit 1
    }
  '';

  # The tools that a build with an aihc compiler needs: a C toolchain, the
  # preprocessors of the Hackage packages, and lz4 for the embedded core
  # libraries of a release.
  selfCompileInputs =
    [
      pkgs.bash
      pkgs.cacert
      pkgs.coreutils
      pkgs.findutils
      pkgs.gnutar
      pkgs.gzip
      pkgs.gawk
      pkgs.gnused
      pkgs.lz4
      pkgs.llvmPackages.bintools
      pkgs.llvmPackages.clang
      pkgs.haskellPackages.hsc2hs
      pkgs.haskellPackages.happy
      pkgs.haskellPackages.alex
      pkgs.haskellPackages.c2hs
    ]
    ++ wasmSpliceInputs;

  # Template Haskell splices and quasi-quotes run as wasm32-wasip3 code. To
  # compile them, aihc needs an unwrapped clang, the WASI sysroot, and the
  # wasm linkers. The Nix checks of the Hackage packages use the same tools.
  wasmSpliceInputs = [
    pkgs.llvmPackages.clang-unwrapped
    pkgs.wasm-tools
    pkgs.wasm-component-ld
    wasmLd
  ];
  wasmSpliceEnvironment = ''
    export AIHC_WASM_CLANG=${pkgs.llvmPackages.clang-unwrapped}/bin/clang
    export AIHC_WASM_SYSROOT=${wasmSysroot}
  '';

  selfCompileEnvironment = ''
    set -euo pipefail
    ${repoRootGuard}
    export GHCRTS=-N
    export LANG=C.UTF-8
    export LC_ALL=C.UTF-8
    export CPATH="${pkgs.lib.makeSearchPathOutput "dev" "include" (selfCompileCLibraries ++ [staticZlib])}''${CPATH:+:$CPATH}"
    export LIBRARY_PATH="${staticZlib.static}/lib:${pkgs.lib.makeLibraryPath selfCompileCLibraries}''${LIBRARY_PATH:+:$LIBRARY_PATH}"
    export SSL_CERT_FILE="''${SSL_CERT_FILE:-${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt}"
    ${wasmSpliceEnvironment}
  '';

  # The Linux release executable is a static musl executable. It has no
  # program interpreter and loads no shared library, so it runs on each
  # Linux distribution. The Nix glibc is not usable for this: an executable
  # that links it needs the program interpreter of the Nix store.
  #
  # aihc runs `clang` with --target=x86_64-unknown-linux-gnu, and with -pie
  # or -no-pie. This clang replaces the target with musl and links
  # statically. Static linking with libc++ needs the unwinder of LLVM.
  muslStatic = pkgs.pkgsStatic;
  muslClang = muslStatic.buildPackages.llvmPackages.clang;
  releaseMuslClang = pkgs.writeShellApplication {
    name = "clang";
    text = ''
      link=1
      arguments=()
      for argument in "$@"; do
        case "$argument" in
        -c | -S | -E | -M | -MM | -fsyntax-only | --version | -### | -print-* | --print-*) link=0 ;;
        esac
        case "$argument" in
        --target=x86_64-unknown-linux-gnu | -pie | -no-pie) ;;
        *) arguments+=("$argument") ;;
        esac
      done
      if [ "$link" -eq 1 ]; then
        arguments+=(-static --rtlib=compiler-rt --unwindlib=libunwind "-L${muslStatic.llvmPackages.libunwind}/lib")
      fi
      exec ${muslClang}/bin/${muslClang.targetPrefix}clang "''${arguments[@]}"
    '';
  };

  # The tools and the environment of a release build. On Linux, the static
  # musl clang comes first on PATH, and the C libraries are the static musl
  # libraries. On macOS, a release build is the same as a self-compile.
  releaseCompileInputs =
    if pkgs.stdenv.hostPlatform.isLinux
    then [releaseMuslClang] ++ selfCompileInputs
    else selfCompileInputs;
  releaseCompileEnvironment =
    if pkgs.stdenv.hostPlatform.isLinux
    then ''
      set -euo pipefail
      ${repoRootGuard}
      export GHCRTS=-N
      export LANG=C.UTF-8
      export LC_ALL=C.UTF-8
      export CPATH="${muslStatic.zlib.dev}/include"
      export LIBRARY_PATH="${muslStatic.zlib}/lib"
      export SSL_CERT_FILE="''${SSL_CERT_FILE:-${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt}"
      ${wasmSpliceEnvironment}
    ''
    else selfCompileEnvironment;

  mkAppWithInputs = name: runtimeInputs: text: {
    type = "app";
    program = "${pkgs.writeShellApplication {
      inherit name runtimeInputs text;
    }}/bin/${name}";
    meta.description = "aihc app: ${name}";
  };

  mkApp = name: text: mkAppWithInputs name [pkgs.bash pkgs.cabal-install hsPkgs.ghc] text;

  mkFmtApp = name: text:
    mkAppWithInputs name [pkgs.bash pkgs.git pkgs.findutils pkgs.alejandra (projectHsPackages pkgs).ormolu] text;

  mkReportsApp = name: text: {
    type = "app";
    program = "${pkgs.writeShellApplication {
      inherit name text;
      runtimeInputs = [pkgs.bash pkgs.nix];
    }}/bin/${name}";
    meta.description = "aihc app: ${name}";
  };

  mkComponentApp = name: component: text:
    mkApp name ''
      set -euo pipefail
      ${repoRootGuard}
      cd ${component}
      ${text}
    '';

  aihcApp = mkAppWithInputs "aihc" [pkgs.bash] ''
    exec ${aihcExe} +RTS -M4G -RTS "$@"
  '';
in {
  fmt = mkFmtApp "fmt" ''
    set -euo pipefail

    repo_root="$(git rev-parse --show-toplevel 2>/dev/null)" || {
      echo "Run this app from inside the repository." >&2
      exit 1
    }
    cd "$repo_root"

    git ls-files -z -- '*.nix' | xargs -0 -r alejandra
    git ls-files -z -- '*.hs' | grep -vz '/Fixtures/' | xargs -0 -r ormolu -m inplace
  '';

  generate-unicode = mkAppWithInputs "generate-unicode" [pkgs.bash pkgs.git pkgs.ormolu] ''
    set -euo pipefail
    ${repoRootGuard}

    repo_root="$(git rev-parse --show-toplevel)"
    cd "$repo_root"
    output="''${AIHC_UNICODE_OUTPUT:-$repo_root/core-libs/aihc-prim/src/GHC/Prim/Unicode.hs}"
    generated_dir="$(mktemp -d)"
    trap 'rm -rf "$generated_dir"' EXIT

    UNICODE_VERSION=${unicode.version} ${unicode.generator} \
      --input=${unicode.ucd}/ \
      --output="$generated_dir" \
      --core-prop=Uppercase \
      --core-prop=Lowercase
    cp "$generated_dir/GHC/Prim/Unicode.hs" "$output"
    ormolu --mode inplace "$output"
    echo "Generated $output from Unicode ${unicode.version}."
  '';

  check-unicode = mkAppWithInputs "check-unicode" [pkgs.bash pkgs.coreutils pkgs.diffutils pkgs.git pkgs.ormolu] ''
    set -euo pipefail
    ${repoRootGuard}

    repo_root="$(git rev-parse --show-toplevel)"
    cd "$repo_root"
    committed="$repo_root/core-libs/aihc-prim/src/GHC/Prim/Unicode.hs"
    generated_dir="$(mktemp -d)"
    generated="$generated_dir/GHC/Prim/Unicode.hs"
    trap 'rm -rf "$generated_dir"' EXIT

    UNICODE_VERSION=${unicode.version} ${unicode.generator} \
      --input=${unicode.ucd}/ \
      --output="$generated_dir" \
      --core-prop=Uppercase \
      --core-prop=Lowercase
    ormolu --mode inplace "$generated"

    if ! cmp --silent "$committed" "$generated"; then
      echo "Committed Unicode tables are stale. Run: just generate-unicode" >&2
      diff --unified "$committed" "$generated" || true
      exit 1
    fi
    echo "Committed Unicode tables match Unicode ${unicode.version}."
  '';

  aihc-dev = mkAppWithInputs "aihc-dev" [pkgs.bash hsPkgs.ghc] ''
    exec ${aihcDevExe} "$@"
  '';

  aihc = aihcApp;

  resolve-progress = mkComponentApp "resolve-progress" "components/aihc-resolve" ''
    ${resolveProgressExe} "$@"
  '';

  resolve-progress-strict = mkComponentApp "resolve-progress-strict" "components/aihc-resolve" ''
    ${resolveProgressExe} --strict "$@"
  '';

  resolve-extension-progress = mkComponentApp "resolve-extension-progress" "components/aihc-resolve" ''
    ${resolveExtensionProgressExe} "$@"
  '';

  fixture-extension-progress = mkAppWithInputs "fixture-extension-progress" [pkgs.bash] ''
    set -euo pipefail
    ${repoRootGuard}
    exec ${fixtureExtensionProgressExe} "$@"
  '';

  fixture-progress = mkAppWithInputs "fixture-progress" [pkgs.bash] ''
    set -euo pipefail
    ${repoRootGuard}
    exec ${fixtureProgressExe} "$@"
  '';

  tc-progress = mkComponentApp "tc-progress" "components/aihc-tc" ''
    ${tcProgressExe} "$@"
  '';

  tc-progress-strict = mkComponentApp "tc-progress-strict" "components/aihc-tc" ''
    ${tcProgressExe} --strict "$@"
  '';

  tc-test = mkComponentApp "tc-test" "components/aihc-tc" ''
    cabal test --test-show-details=direct
  '';

  install-hackage-packages =
    mkAppWithInputs "install-hackage-packages" [
      pkgs.bash
      pkgs.cacert
      pkgs.coreutils
      pkgs.curl
      pkgs.gnutar
      pkgs.gzip
      pkgs.gawk
      pkgs.gnused
      pkgs.llvmPackages.bintools
      pkgs.llvmPackages.clang
      pkgs.llvmPackages.clang-unwrapped
      pkgs.wasm-tools
      pkgs.wasm-component-ld
      wasmLd
      pkgs.haskellPackages.hsc2hs
      pkgs.haskellPackages.happy
      pkgs.haskellPackages.alex
      pkgs.haskellPackages.c2hs
    ] ''
      set -euo pipefail
      ${repoRootGuard}
      export GHCRTS=-N
      export LANG=C.UTF-8
      export LC_ALL=C.UTF-8
      export AIHC=${aihcExe}
      # The wasm32-wasip3 target compiles its C runtime with an unwrapped
      # clang against the WASI sysroot, the same way the Nix checks do.
      export AIHC_WASM_CLANG=${pkgs.llvmPackages.clang-unwrapped}/bin/clang
      export AIHC_WASM_SYSROOT=${wasmSysroot}
      export SSL_CERT_FILE="''${SSL_CERT_FILE:-${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt}"
      exec bash ./scripts/install-hackage-packages.sh "$@"
    '';

  # The web kit: aihc-play compiled by aihc to wasm32-wasip3 and transpiled
  # by jco, with the browser tools and the JavaScript glue. The build reads
  # Hackage for the dependencies of aihc-play, so it is an app and not a
  # derivation. The release workflow packs the output. The deploy workflow
  # adds the output to the site of the guide.
  build-web-kit =
    mkAppWithInputs "build-web-kit" [
      pkgs.bash
      pkgs.cacert
      pkgs.coreutils
      pkgs.curl
      pkgs.findutils
      pkgs.gnutar
      pkgs.gzip
      pkgs.gawk
      pkgs.gnused
      pkgs.llvmPackages.bintools
      pkgs.llvmPackages.clang
      pkgs.llvmPackages.clang-unwrapped
      pkgs.wasm-tools
      pkgs.wasm-component-ld
      wasmLd
      pkgs.binaryen
      pkgs.nodejs_24
    ] ''
      set -euo pipefail
      ${repoRootGuard}
      export GHCRTS=-N
      export LANG=C.UTF-8
      export LC_ALL=C.UTF-8
      export AIHC="''${AIHC:-${aihcExe}}"
      export AIHC_PLAY_TOOLS=${playTools}
      export AIHC_DEV="''${AIHC_DEV:-${aihcDevExe}}"
      export AIHC_WASM_CLANG=${pkgs.llvmPackages.clang-unwrapped}/bin/clang
      export AIHC_WASM_SYSROOT=${wasmSysroot}
      export SSL_CERT_FILE="''${SSL_CERT_FILE:-${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt}"
      exec bash ./scripts/build-web-kit.sh "$@"
    '';

  publish-play-tools = mkAppWithInputs "publish-play-tools" [pkgs.bash pkgs.curl pkgs.coreutils pkgs.wrangler] ''
    export AIHC_PLAY_TOOLS=${playTools}
    exec bash ./scripts/publish-play-tools.sh "$@"
  '';

  # Load a kit in a headless browser, compile a program, and run it. The
  # browser is Chromium from the playwright-driver package of the pinned
  # nixpkgs. The test starts it directly and does not use Playwright.
  web-kit-smoke-test = mkAppWithInputs "web-kit-smoke-test" [pkgs.bash pkgs.coreutils pkgs.gnutar pkgs.gzip pkgs.nodejs_24] ''
    set -euo pipefail
    ${repoRootGuard}
    export AIHC_SMOKE_BROWSERS=${smokeBrowsers}
    exec node ./web-kit/test/smoke.mjs "$@"
  '';

  # A release publishes its kit to R2 under its version.
  publish-web-kit = mkAppWithInputs "publish-web-kit" [pkgs.bash pkgs.curl pkgs.coreutils pkgs.diffutils pkgs.jq pkgs.wrangler] ''
    exec bash ./scripts/publish-web-kit.sh "$@"
  '';

  # The documentation preview publishes the compressed helper files to R2
  # and keeps only the small files on the preview branch.
  publish-play-helper = mkAppWithInputs "publish-play-helper" [pkgs.bash pkgs.curl pkgs.coreutils pkgs.jq pkgs.wrangler] ''
    exec bash ./scripts/publish-play-helper.sh "$@"
  '';

  self-hosting-packages =
    mkAppWithInputs "self-hosting-packages" [
      pkgs.bash
      pkgs.cacert
      pkgs.coreutils
      pkgs.gawk
    ] ''
      set -euo pipefail
      ${repoRootGuard}
      export AIHC=${aihcExe}
      export SSL_CERT_FILE="''${SSL_CERT_FILE:-${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt}"
      exec bash ./scripts/update-self-hosting-packages.sh "$@"
    '';

  self-hosting-progress =
    mkAppWithInputs "self-hosting-progress" [
      pkgs.bash
      pkgs.cacert
      pkgs.coreutils
      pkgs.curl
      pkgs.gnutar
      pkgs.gzip
      pkgs.gawk
      pkgs.gnused
      pkgs.pkg-config
      pkgs.llvmPackages.bintools
      pkgs.llvmPackages.clang
      pkgs.haskellPackages.hsc2hs
      pkgs.haskellPackages.happy
      pkgs.haskellPackages.alex
      pkgs.haskellPackages.c2hs
    ] ''
      set -euo pipefail
      ${repoRootGuard}
      export GHCRTS=-N
      export LANG=C.UTF-8
      export LC_ALL=C.UTF-8
      export AIHC=${aihcExe}
      export CPATH="${pkgs.lib.makeSearchPathOutput "dev" "include" selfHostingCLibraries}''${CPATH:+:$CPATH}"
      export LIBRARY_PATH="${pkgs.lib.makeLibraryPath selfHostingCLibraries}''${LIBRARY_PATH:+:$LIBRARY_PATH}"
      export PKG_CONFIG_PATH="${pkgConfigPath selfHostingCLibraries}''${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"
      export SSL_CERT_FILE="''${SSL_CERT_FILE:-${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt}"
      exec bash ./scripts/self-hosting-progress.sh "$@"
    '';

  # Compile new Hackage packages with GHC and aihc, and add the results to
  # docs/hackage-survey.md. The GHC build uses the cabal-install and GHC of
  # the project. The C libraries of surveyCLibraries and pkg-config serve
  # packages that bind to a C library.
  hackage-survey =
    mkAppWithInputs "hackage-survey" [
      pkgs.bash
      pkgs.cabal-install
      pkgs.cacert
      pkgs.coreutils
      pkgs.curl
      pkgs.findutils
      pkgs.gawk
      pkgs.gnugrep
      pkgs.gnused
      pkgs.pkg-config
      pkgs.llvmPackages.bintools
      pkgs.llvmPackages.clang
      pkgs.haskellPackages.hsc2hs
      pkgs.haskellPackages.happy
      pkgs.haskellPackages.alex
      pkgs.haskellPackages.c2hs
      # The configure script of postgresql-libpq runs pg_config.
      pkgs.libpq.pg_config
      hsPkgs.ghc
    ] ''
      set -euo pipefail
      ${repoRootGuard}
      # GHCRTS=-N would stop the ghc-pkg of GHC, which is not threaded, so
      # the script gives the option to aihc only.
      export AIHC_GHCRTS=-N
      export LANG=C.UTF-8
      export LC_ALL=C.UTF-8
      export AIHC="''${AIHC:-${aihcExe}}"
      export CPATH="${pkgs.lib.makeSearchPathOutput "dev" "include" surveyCLibraries}''${CPATH:+:$CPATH}"
      export LIBRARY_PATH="${pkgs.lib.makeLibraryPath surveyCLibraries}''${LIBRARY_PATH:+:$LIBRARY_PATH}"
      export PKG_CONFIG_PATH="${pkgConfigPath surveyCLibraries}''${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"
      export SSL_CERT_FILE="''${SSL_CERT_FILE:-${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt}"
      exec bash ./scripts/hackage-survey.sh "$@"
    '';

  # One stage of the self-compile. Stage 0 is the GHC-built aihc, so AIHC
  # defaults to it. To build a later stage, set AIHC to the previous stage.
  self-compile-stage = mkAppWithInputs "self-compile-stage" selfCompileInputs ''
    ${selfCompileEnvironment}
    export AIHC="''${AIHC:-${aihcExe}}"
    exec bash ./scripts/self-compile-stage.sh "$@"
  '';

  # Build one stage of a release executable. The options are the options of
  # self-compile-stage. On Linux, the executable is a static musl executable.
  release-compile-stage = mkAppWithInputs "release-compile-stage" releaseCompileInputs ''
    ${releaseCompileEnvironment}
    export AIHC="''${AIHC:-${aihcExe}}"
    exec bash ./scripts/self-compile-stage.sh "$@"
  '';

  # Build and run one example with a release executable. The --aihc option
  # names the executable, because a release does not come from the Nix build.
  release-smoke-test = mkAppWithInputs "release-smoke-test" selfCompileInputs ''
    ${selfCompileEnvironment}
    exec bash ./scripts/release-smoke-test.sh "$@"
  '';

  generate-reports = mkReportsApp "generate-reports" ''
    set -euo pipefail
    ${repoRootGuard}
    bash ./scripts/update-generated-content.sh --update
  '';

  check-reports = mkReportsApp "check-reports" ''
    set -euo pipefail
    ${repoRootGuard}
    bash ./scripts/update-generated-content.sh --check
  '';

  default = aihcApp;
}

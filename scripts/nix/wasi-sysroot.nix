# The WASI sysroot the wasm32-wasip3 target compiles and links against.
#
# It is the libc that wasi-sdk builds for WASI 0.3, and not the wasi-libc of
# nixpkgs, which is built for preview 1: that libc calls the host through
# imports that the component of a program cannot have. The sysroot is the
# release asset of wasi-sdk, pinned by hash, and cut down to the headers and
# archives of the one target. The compiler runtime archive of the target is a
# separate asset of the same release, and goes beside the libc, where the
# compiler looks for it.
pkgs: let
  release = "https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-34";
  sysroot = pkgs.fetchurl {
    url = "${release}/wasi-sysroot-34.0.tar.gz";
    hash = "sha256-nYE1RO7r44t7jyJE7Vkd5GttuBLG3Rolf/nw0qkFor4=";
  };
  compilerRuntime = pkgs.fetchurl {
    url = "${release}/libclang_rt-34.0.tar.gz";
    hash = "sha256-7uPmNNz3GqIrEzM5FiPPXJllpjfcQoonsahYwCbFh/E=";
  };
in
  pkgs.runCommand "aihc-wasi-sysroot" {nativeBuildInputs = [pkgs.gnutar pkgs.gzip];} ''
    mkdir unpacked
    tar -xzf ${sysroot} -C unpacked
    tar -xzf ${compilerRuntime} -C unpacked
    mkdir -p "$out/include" "$out/lib"
    cp -r unpacked/wasi-sysroot-34.0/include/wasm32-wasip3 "$out/include/"
    cp -r unpacked/wasi-sysroot-34.0/lib/wasm32-wasip3 "$out/lib/"
    chmod -R u+w "$out"
    cp unpacked/libclang_rt-34.0/wasm32-unknown-wasip3/libclang_rt.builtins.a \
      "$out/lib/wasm32-wasip3/"
    test -e "$out/include/wasm32-wasip3/stdlib.h"
    test -e "$out/lib/wasm32-wasip3/libc.a"
    test -e "$out/lib/wasm32-wasip3/libclang_rt.builtins.a"
  ''

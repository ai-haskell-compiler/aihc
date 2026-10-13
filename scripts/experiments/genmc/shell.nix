let
  project = builtins.getFlake (toString ../../..);
  pkgs = import project.inputs.nixpkgs {system = builtins.currentSystem;};
  llvm = pkgs.llvmPackages_19;
  genmc = llvm.stdenv.mkDerivation {
    pname = "genmc";
    version = "0.19.0";
    src = pkgs.fetchzip {
      url = "https://github.com/MPI-SWS/genmc/archive/9f6c4c0772d0c42b325681581acc6a0f3eb9b5f7.tar.gz";
      sha256 = "0l7sbbhfqb5gimg47477j3dmadwc7nz110ahpnhv670x2pk2j96b";
    };
    nativeBuildInputs = [pkgs.cmake pkgs.ninja llvm.llvm.dev];
    buildInputs = [pkgs.libffi pkgs.libedit pkgs.zlib];
    cmakeFlags = [
      "-DCMAKE_CXX_SCAN_FOR_MODULES=OFF"
      "-DCLANGPATH=${llvm.clang}/bin/clang"
      "-DLLVM_CONFIG_PATH=${llvm.llvm.dev}/bin/llvm-config"
    ];
    enableParallelBuilding = true;
  };
in
  pkgs.mkShell {
    packages = [genmc llvm.clang llvm.llvm pkgs.python3];
    GENMC_INCLUDE = "${genmc}/include/genmc/runtime";
  }

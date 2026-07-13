{pkgs}: let
  version = "17.0.0";
  ucd2haskellRevision = "4396a6f2a4c7799908e1e0b88a218a51d063fdca";
  ucd2haskellArchive = pkgs.fetchzip {
    name = "ghc-ucd2haskell-${ucd2haskellRevision}.tar.gz";
    url = "https://gitlab.haskell.org/ghc/ghc/-/archive/${ucd2haskellRevision}/ghc-${ucd2haskellRevision}.tar.gz?path=libraries/ghc-internal/tools/ucd2haskell";
    extension = "tar.gz";
    hash = "sha256-5FDNk4R18w1t0NjvJHW21je+GoB+6YJ39jtltDlPphU=";
  };
  ucd2haskellSource = pkgs.runCommand "ghc-ucd2haskell-aihc-source" {} ''
    cp -R ${ucd2haskellArchive}/libraries/ghc-internal/tools/ucd2haskell "$out"
    chmod -R u+w "$out"
    cp ${./ucd2haskell-aihc/Main.hs} "$out/exe/UCD2Haskell.hs"
    cp ${./ucd2haskell-aihc/AIHC.hs} "$out/exe/UCD2Haskell/AIHC.hs"
    substituteInPlace "$out/ucd2haskell.cabal" \
      --replace-fail \
      '    UCD2Haskell.ModuleGenerators' \
      '    UCD2Haskell.ModuleGenerators
        UCD2Haskell.AIHC'
  '';
  ucd2haskell = pkgs.haskell.lib.dontHaddock (
    pkgs.haskellPackages.callCabal2nix "ucd2haskell" ucd2haskellSource {}
  );
  unicodeData = pkgs.fetchurl {
    url = "https://www.unicode.org/Public/${version}/ucd/UnicodeData.txt";
    hash = "sha256-Lh78HctZxXXu31zK5g+VIp9wbubQMYNSR9hDwR2WRww=";
  };
  derivedCoreProperties = pkgs.fetchurl {
    url = "https://www.unicode.org/Public/${version}/ucd/DerivedCoreProperties.txt";
    hash = "sha256-JMf+0RlcSC+q79XB5+uCHF7h+23gfs26pktWqZ2iLAg=";
  };
in {
  inherit version ucd2haskellRevision;

  generator = pkgs.lib.getExe' ucd2haskell "ucd2haskell";

  ucd = pkgs.linkFarm "unicode-ucd-${version}" [
    {
      name = "UnicodeData.txt";
      path = unicodeData;
    }
    {
      name = "DerivedCoreProperties.txt";
      path = derivedCoreProperties;
    }
  ];
}

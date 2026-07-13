{
  pkgs,
  hsPkgs,
}: let
  version = "17.0.0";
in {
  inherit version;

  generator = pkgs.lib.getExe' hsPkgs.aihc-ucd2haskell "ucd2haskell";

  unicodeData = pkgs.fetchurl {
    url = "https://www.unicode.org/Public/${version}/ucd/UnicodeData.txt";
    hash = "sha256-Lh78HctZxXXu31zK5g+VIp9wbubQMYNSR9hDwR2WRww=";
  };

  derivedCoreProperties = pkgs.fetchurl {
    url = "https://www.unicode.org/Public/${version}/ucd/DerivedCoreProperties.txt";
    hash = "sha256-JMf+0RlcSC+q79XB5+uCHF7h+23gfs26pktWqZ2iLAg=";
  };
}

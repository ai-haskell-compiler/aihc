{ lib, stdenv, fetchFromGitHub, pkg-config, lapack }:

stdenv.mkDerivation rec {
  version = "2.11.10-latest";
  pname = "coinutils";

  src = fetchFromGitHub {
    owner = "coin-or";
    repo = "CoinUtils";
    rev = "293e6e981774ed047e8f00f7aff9252262f83a02";
    hash = "sha256-H3GBO5voOJO/0gMC0QY3me1qQFsCcOg5SuuoI3YV0IE=";
  };

  nativeBuildInputs = [ pkg-config lapack ];

  doCheck = true;

  meta = with lib; {
    license = licenses.epl20;
    homepage = "https://github.com/coin-or/CoinUtils";
    description = "Collection of classes and helper functions that are generally useful to multiple COIN-OR projects";
    maintainers = with maintainers; [ tmarkus ];
  };
}

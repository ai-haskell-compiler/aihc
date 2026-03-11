{ lib, stdenv, fetchFromGitHub, pkg-config, coin-utils, zlib, osi }:

stdenv.mkDerivation rec {
  version = "1.17.9-latest";
  pname = "clp";
  src = fetchFromGitHub {
    owner = "coin-or";
    repo = "Clp";
    rev = "c9062fc02ccac08248cbf7b3ffae02994d7ea8c2";
    hash = "sha256-vQ4X+8O5FyXjlHvxlfS6E1vA29ssvGes3pLkg5Zbdfo=";
  };

  patches = [ ./clp-extern-c.patch ];

  nativeBuildInputs = [ pkg-config ];

  propagatedBuildInputs = [ zlib coin-utils osi ];

  doCheck = true;

  meta = with lib; {
    license = licenses.epl20;
    homepage = "https://github.com/coin-or/Clp";
    description = "An open-source linear programming solver written in C++";
    platforms = platforms.darwin ++ platforms.linux;
    maintainers = [ maintainers.vbgl ];
  };
}

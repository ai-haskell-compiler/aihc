{ mkDerivation, base, base16-bytestring, bytestring, c2hs, hpack, lib, libnfc }:
mkDerivation rec {
  pname = "nfc";
  version = "0.1.1";
  src = ../.;

  isLibrary = true;
  libraryHaskellDepends = [ base bytestring ];
  libraryPkgconfigDepends = [ libnfc ];
  libraryToolDepends = [ c2hs hpack ];

  isExecutable = true;
  executableHaskellDepends = [ base base16-bytestring bytestring ];
  configureFlags = [ "-fbuild-examples" ];
  postFixup = "mv $out/bin $doc/bin";

  prePatch = "hpack";

  homepage = "https://github.com/centromere/nfc#readme";
  description = "libnfc bindings";
  license = lib.licenses.publicDomain;
  maintainers = with lib.maintainers; [ centromere ];
}

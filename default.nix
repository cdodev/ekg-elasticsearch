{ mkDerivation, aeson, base, bytestring, ekg-core, hostname, lens
, stdenv, text, time, unordered-containers, wreq
}:
mkDerivation {
  pname = "ekg-elastic";
  version = "0.2.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring ekg-core hostname lens text time
    unordered-containers wreq
  ];
  homepage = "https://github.com/cdodev/ekg-elastic";
  description = "Push metrics to elastic";
  license = stdenv.lib.licenses.bsd3;
}

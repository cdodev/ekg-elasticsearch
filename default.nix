{ mkDerivation, aeson, base, bytestring, ekg-core, hostname
, http-client, lens, stdenv, text, time, unordered-containers, wreq
}:
mkDerivation {
  pname = "ekg-elasticsearch";
  version = "0.2.2.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring ekg-core hostname http-client lens text time
    unordered-containers wreq
  ];
  homepage = "https://github.com/cdodev/ekg-elasticsearch";
  description = "Push metrics to elasticsearch";
  license = stdenv.lib.licenses.bsd3;
}

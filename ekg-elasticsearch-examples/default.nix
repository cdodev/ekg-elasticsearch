{ mkDerivation, base, ekg-core, ekg-elasticsearch, stdenv }:
mkDerivation {
  pname = "ekg-elasticsearch-examples";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ekg-core ekg-elasticsearch ];
  license = stdenv.lib.licenses.bsd3;
}

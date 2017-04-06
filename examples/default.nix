{ mkDerivation, base, ekg-core, ekg-elastic, stdenv }:
mkDerivation {
  pname = "ekg-elastic-examples";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ekg-core ekg-elastic ];
  license = stdenv.lib.licenses.bsd3;
}

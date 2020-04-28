{ mkDerivation, base, pure-time, stdenv }:
mkDerivation {
  pname = "pure-cached";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure-time ];
  homepage = "github.com/grumply/pure-cached";
  description = "Caching facility";
  license = stdenv.lib.licenses.bsd3;
}

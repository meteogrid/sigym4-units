{ mkDerivation, base, data-default, dimensional, ieee754
, sigym4-null, stdenv
}:
mkDerivation {
  pname = "sigym4-units";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base data-default dimensional ieee754 sigym4-null
  ];
  homepage = "https://github.com/meteogrid/sigym4-units";
  description = "Dimensional types for Sigym4";
  license = stdenv.lib.licenses.bsd3;
}

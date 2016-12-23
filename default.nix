{ mkDerivation, base, data-default, deepseq, dimensional
, geohs-fingerprint, ieee754, newtype, sigym4-null, stdenv
, template-haskell
}:
mkDerivation {
  pname = "sigym4-units";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base data-default deepseq dimensional geohs-fingerprint ieee754
    newtype sigym4-null template-haskell
  ];
  homepage = "https://github.com/meteogrid/sigym4-units";
  description = "Dimensional types for Sigym4";
  license = stdenv.lib.licenses.bsd3;
}

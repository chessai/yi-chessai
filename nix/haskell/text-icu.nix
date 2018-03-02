{ mkDerivation, array, base, bytestring, deepseq, directory
, fetchgit, ghc-prim, HUnit, icu, QuickCheck, random, stdenv
, test-framework, test-framework-hunit, test-framework-quickcheck2
, text
}:
mkDerivation {
  pname = "text-icu";
  version = "0.55.1.0";
  src = fetchgit {
    url = "https://github.com/bos/text-icu.git";
    sha256 = "12gs3rn1yfzmaqxfbaxzp2xy0ai0sqnw9dc3wb61byd8lvpx73kz";
    rev = "5ab84d8106c06fbabb87a0f173faa6049775a148";
  };
  libraryHaskellDepends = [ base bytestring deepseq text ];
  librarySystemDepends = [ icu ];
  testHaskellDepends = [
    array base bytestring deepseq directory ghc-prim HUnit QuickCheck
    random test-framework test-framework-hunit
    test-framework-quickcheck2 text
  ];
  homepage = "https://github.com/bos/text-icu";
  description = "Bindings to the ICU library";
  license = stdenv.lib.licenses.bsd3;
}

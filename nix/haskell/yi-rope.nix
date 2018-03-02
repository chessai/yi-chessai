{ mkDerivation, base, binary, bytestring, criterion, deepseq
, fetchgit, fingertree, hspec, QuickCheck, quickcheck-instances
, stdenv, text
}:
mkDerivation {
  pname = "yi-rope";
  version = "0.10";
  src = fetchgit {
    url = "https://github.com/yi-editor/yi-rope.git";
    sha256 = "0lxpyq0bsf8ii6q5zq87z5a3fxya3m54gr333vd9j8cjmddi2zbv";
    rev = "fc711de176aa1453cd197826c2f84a5b21c9ca51";
  };
  libraryHaskellDepends = [
    base binary bytestring deepseq fingertree text
  ];
  testHaskellDepends = [
    base hspec QuickCheck quickcheck-instances text
  ];
  benchmarkHaskellDepends = [ base criterion deepseq text ];
  description = "A rope data structure used by Yi";
  license = stdenv.lib.licenses.gpl2;
}

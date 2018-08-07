{ mkDerivation, base, binary, bytestring, criterion, deepseq
, fetchgit, fingertree, hspec, QuickCheck, quickcheck-instances
, stdenv, text
}:
mkDerivation {
  pname = "yi-rope";
  version = "0.11";
  src = fetchgit {
    url = "https://github.com/yi-editor/yi-rope";
    sha256 = "0vnjrvaknz5jfgg0na6nqfmmywm118c170782xhjkiqi07cbp7c2";
    rev = "f3b925e2f4c55092957cecc3c037f36baff582bb";
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

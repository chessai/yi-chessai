{ mkDerivation, base, binary, bytestring, fetchgit, hashable
, stdenv, unordered-containers
}:
mkDerivation {
  pname = "dynamic-state";
  version = "0.3.1";
  src = fetchgit {
    url = "https://github.com/yi-editor/dynamic-state";
    sha256 = "0967796j9rls300k7mz155zf4ybk8540na4x4shkqcyc3l7q9s8n";
    rev = "7ebe5f304d63925980a6e8db1f8badd51afb278b";
  };
  libraryHaskellDepends = [
    base binary bytestring hashable unordered-containers
  ];
  description = "Optionally serializable dynamic state keyed by type";
  license = stdenv.lib.licenses.gpl2;
}

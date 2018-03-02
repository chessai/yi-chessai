{ mkDerivation, base, binary, bytestring, fetchgit, hashable
, stdenv, unordered-containers
}:
mkDerivation {
  pname = "dynamic-state";
  version = "0.3";
  src = fetchgit {
    url = "https://github.com/yi-editor/dynamic-state.git";
    sha256 = "19dravi3q1a08hv88vxk9n6p3r5wijf6bxkcy0qcyghgv6f791xj";
    rev = "47aeb4d4d8c7b35278e733afb92e652d9d3ebbbc";
  };
  libraryHaskellDepends = [
    base binary bytestring hashable unordered-containers
  ];
  description = "Optionally serializable dynamic state keyed by type";
  license = stdenv.lib.licenses.gpl2;
}

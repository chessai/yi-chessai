{ owner  # The owner of the nixpkgs
, rev    # The Git revision of nixpkgs to fetch
, sha256 # The SHA256 hash of the unpacked archive
}:

builtins.fetchTarball {
  url = "https://github.com/${owner}/nixpkgs/archive/${rev}.tar.gz";
  inherit sha256;
}
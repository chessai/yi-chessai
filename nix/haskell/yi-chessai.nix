{ mkDerivation, base, lens, microlens-platform, mtl, stdenv
, yi-core, yi-frontend-vty, yi-mode-haskell
}:
mkDerivation {
  pname = "yi-chessai";
  version = "0.1.0.0";
  src = builtins.filterSource (path: type: (builtins.substring 0 1 (baseNameOf path) != ".") && (baseNameOf path != "yi.nix")) ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base lens microlens-platform mtl yi-core yi-frontend-vty
    yi-mode-haskell
  ];
  license = stdenv.lib.licenses.mit;
}

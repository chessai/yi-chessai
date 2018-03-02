{ package ? "yi-chessai", compiler ? "ghc822" }:
(import ./default.nix {
  inherit package compiler;
}).yi-chessai

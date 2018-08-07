{ package ? "yi-chessai" , compiler ? "ghc843" }:

let _nixpkgs = import <nixpkgs> {};
    nixpkgs = _nixpkgs.fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs";
      rev = "5c4a404b0d0e5125070dde5c1787210149157e83";
      sha256 = "0a478l0dxzy5hglavkilxjkh45zfg31q50hgkv1npninc4lpv5f7";
    };
    pkgs = import nixpkgs { config = {}; overlays = []; };
    inherit (pkgs) haskell;

    fetch-github-json = owner: repo: path:
      let commit = builtins.fromJSON (builtins.readFile path);
      in pkgs.fetchFromGitHub {
        name = "${repo}-${commit.rev}";
          inherit owner repo;
          inherit (commit) rev sha256;
      };

    yi-src = fetch-github-json "yi-editor" "yi" ./nix/haskell/yi.json;
    
    filterPredicate = p: type:
      let path = baseNameOf p; in !(
           (type == "directory" && path == "dist")
        || (type == "symlink"   && path == "result")
        || (type == "directory" && path == ".git")
        || (type == "symlink"   && pkgs.lib.hasPrefix "result" path)
        || pkgs.lib.hasSuffix "~" path
        || pkgs.lib.hasSuffix ".o" path
        || pkgs.lib.hasSuffix ".so" path
        || pkgs.lib.hasSuffix ".nix" path);

    overrides = haskell.packages.${compiler}.override {
      overrides = self: super:
        with haskell.lib; 
        let
          cp = file: (self.callPackage (./nix/haskell + "/${file}.nix") {}); 
          build = name: path: self.callCabal2nix name (builtins.filterSource filterPredicate path) {};
          build-from-json = name: str: self.callCabal2nix name str {}; 
          doMap = fn: list: pkgs.lib.listToAttrs (map fn list);
          yiMapFn = name: {
            inherit name;
            value = build-from-json name "${yi-src}/${name}";
          };
          yi-pkgs = doMap yiMapFn
            [
              "yi"
              "yi-core"
              "yi-keymap-vim"
              "yi-language"
              "yi-frontend-vty"
              "yi-misc-modes"
              "yi-mode-haskell"
              "yi-mode-javascript"
            ]; 
        in  
        yi-pkgs //
        {
          dynamic-state = cp "dynamic-state"; 
          text-icu = dontCheck (cp "text-icu"); 
          
          yi-rope    = cp "yi-rope"; 
          yi-chessai = build "yi-chessai" ./.; 
        };
      };
  
in rec {
  inherit overrides;
  yi-chessai = overrides.yi-chessai;
}

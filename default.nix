{ package ? "yi-chessai" , compiler ? "ghc844" }:

let fetchNixpkgs = import ./nix/fetchNixpkgs.nix;
    nixpkgs = fetchNixpkgs {
      owner = "layer-3-communications";
      rev   = "1c941f6d3ef5c51db61b1af5590ace047231f7c8";
      sha256 = "1xgsjy6vai61gyy7ry13wvskc6n29i6d11xncn6ki24j1m76lavb";
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

    yi-src = fetch-github-json "chessai" "yi" ./nix/haskell/yi.json;
    
    filterPredicate = p: type:
      let path = baseNameOf p; in !(
           (type == "directory" && path == "dist")
        || (type == "symlink"   && path == "result")
        || (type == "directory" && path == ".git")
        || (type == "symlink"   && pkgs.lib.hasPrefix "result" path)
        || pkgs.lib.hasSuffix "~" path
        || pkgs.lib.hasSuffix ".o" path
        || pkgs.lib.hasSuffix ".so" path
        || pkgs.lib.hasSuffix ".nix" path
        || pkgs.lib.hasSuffix ".md" path);

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
  #yi = overrides.yi;
  #yi-core = overrides.yi-core;
  #yi-keymap-vim = overrides.yi-keymap-vim;
  #yi-language = overrides.yi-frontend-vty;
  #yi-misc-modes = overrides.yi-misc-modes;
  #yi-mode-haskell = overrides.yi-mode-haskell;
  #yi-mode-javascript = overrides.yi-mode-javascript;
  #dynamic-state = overrides.dynamic-state;
  #text-icu = overrides.text-icu;
  #yi-rope = overrides.yi-rope;
  yi-chessai = overrides.yi-chessai;
}

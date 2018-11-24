let main = (import ./default.nix {});
in main.yi // main.yi-core // main.yi-keymap-vim // main.yi-language // main.yi-misc-modes // main.yi-mode-haskell // main.yi-mode-javascript // main.dynamic-state // main.text-icu // main.yi-rope // main.yi-chessai

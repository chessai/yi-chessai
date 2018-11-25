# chessai's yi configuration

## Build

You need [nix](https://nixos.org/nix/download.html) to build this.

```sh
git clone https://github.com/chessai/yi-chessai
nix-build && nix-env -i $(readlink -f result)
```

## Configuration Details
This configuration is pretty bare, with mostly just the default vim keybindings.
Aside from file extension-specific configuration support, this configuration also
has an implementation of rainbow-parens.

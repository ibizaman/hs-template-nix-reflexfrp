{ compiler ? "ghc865"
, system ? builtins.currentSystem
, reflex-platform ? fetchTarball "https://github.com/reflex-frp/reflex-platform/archive/v0.6.2.0.tar.gz"
, pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/20.09.tar.gz") {}
}:
(import reflex-platform { inherit system; }).project (
  { pkgs, ... }: {
    useWarp = true;

    packages = {
      backend = ./backend;
    };

    shells = {
      ghc = ["backend"];
    };

    shellToolOverrides = self: super:
      let
        inherit (pkgs.haskell.lib) dontCheck;
      in {
        haskell-ide-engine = null;
      };

    overrides = self: super: {
    };
  }
)

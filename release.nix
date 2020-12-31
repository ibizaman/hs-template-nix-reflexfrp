{ system ? builtins.currentSystem
, reflex-platform ? fetchTarball "https://github.com/reflex-frp/reflex-platform/archive/v0.7.0.0.tar.gz"
, pkgs' ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/20.09.tar.gz") {}
, useWarp ? true
}:
(import reflex-platform {

  inherit system;

  haskellOverlaysPost = [
    # Use system hpack:
    (_: _: { hpack = pkgs'.haskell.packages.${compiler}.callHackage "hpack" "0.32.0" {}; })
  ];

}).project (
  { pkgs, ... }: {
    inherit useWarp;

    packages = {
      common = ./common;
      backend = ./backend;
      frontend = ./frontend;
    };

    shells = {
      ghc = ["common" "backend" "frontend"];
      ghcjs = ["common" "frontend"];
    };

    shellToolOverrides = ghc: super:
      let
        ghc' = pkgs'.haskell.packages.ghc865;
      in {
        haskell-language-server = ghc'.haskell-language-server;
      };

    overrides = self: super: {
    };
  }
)

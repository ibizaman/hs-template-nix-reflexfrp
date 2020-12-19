{ compiler ? "ghc865"
, system ? builtins.currentSystem
, reflex-platform ? fetchTarball "https://github.com/reflex-frp/reflex-platform/archive/v0.6.2.0.tar.gz"
, pkgs' ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/20.09.tar.gz") {}
}:
(import reflex-platform { inherit system; }).project (
  { pkgs, ... }: {
    useWarp = true;

    packages = {
      common = ./common;
      backend = ./backend;
    };

    shells = {
      ghc = ["common" "backend"];
    };

    shellToolOverrides = _ghc: super:
      let
        inherit (pkgs'.haskell.lib) dontCheck;

        ghc = pkgs'.haskell.packages.${compiler};
      in with pkgs'; rec {
        haskell-ide-engine = null;

        stack = dontCheck ((ghc.callHackage "stack" "2.3.1" {}).override rec {
          Cabal = dontCheck (ghc.callHackage "Cabal" "3.0.2.0" {});
          hpack = dontCheck (ghc.hpack.override {
            inherit Cabal;
          });
          rio-prettyprint = dontCheck (ghc.rio-prettyprint.override {
            inherit Cabal;
          });
          hackage-security = dontCheck (ghc.hackage-security.override {
            inherit Cabal;
          });
          http-download = dontCheck (ghc.http-download.override {
            inherit rio-prettyprint;
          });
          pantry = dontCheck (ghc.pantry.override {
            inherit Cabal hackage-security hpack http-download rio-prettyprint;
          });
        });

        hoogle = dontCheck (ghc.callHackage "hoogle" "5.0.18" {});

        haskell-language-server = ghc.haskell-language-server;
      };

    overrides = self: super: {
    };
  }
)

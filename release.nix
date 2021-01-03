{ system ? builtins.currentSystem
, reflex-platform ? fetchTarball "https://github.com/reflex-frp/reflex-platform/archive/v0.7.0.0.tar.gz"
, pkgs' ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/20.09.tar.gz") {}
, useWarp ? true
}:
(import reflex-platform {
  inherit system;
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
        # This function transforms a version string "1.2.3" into "123".
        removeDotsVersion = ghcVersion: builtins.concatStringsSep "" (builtins.splitVersion ghcVersion);

        # Avoid hardcoding the version of ghc.
        version = removeDotsVersion ghc.reflex.compiler.version;
        ghc' = pkgs'.haskell.packages.${"ghc"+version};
      in {
        haskell-language-server = ghc'.haskell-language-server;
      };

    overrides = self: super: {
      # For frontend, 0.3.4 is broken, 0.3.5 does not have the correct
      # upper bound for reflex; also 0.3.6 is not yet on hackage and
      # not even tagged on github. The default derivation builds an
      # Example binary which fails here so I needed to resort to copy
      # the cabal2nix output here and change the version and
      # configureFlags manually.
      servant-reflex = self.callPackage ({ mkDerivation, aeson, base, bytestring, case-insensitive
      , containers, data-default, exceptions, ghcjs-dom, http-api-data
      , http-media, jsaddle, mtl, network-uri, reflex, reflex-dom-core
      , safe, scientific, servant, servant-auth, stdenv
      , string-conversions, text, transformers
      }:
      mkDerivation {
        pname = "servant-reflex";
        version = "0.3.6";
        src = (fetchGit {url = "https://github.com/imalsogreg/servant-reflex.git"; ref = "master";});
        configureFlags = [];# [ "-fexample" ];
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base bytestring case-insensitive containers data-default exceptions
          ghcjs-dom http-api-data http-media jsaddle mtl network-uri reflex
          reflex-dom-core safe servant servant-auth string-conversions text
          transformers
        ];
        executableHaskellDepends = [
          aeson base reflex reflex-dom-core scientific servant text
        ];
        description = "servant API generator for reflex apps";
        license = stdenv.lib.licenses.bsd3;
      }) {};
    };
  }
)

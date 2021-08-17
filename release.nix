{ system ? builtins.currentSystem
# Syntax to pin a release:
# , reflex-platform ? fetchTarball "https://github.com/reflex-frp/reflex-platform/archive/v0.7.0.0.tar.gz"
# Syntax to use a local folder:
# , reflex-platform ? ../reflex-platform
# Set to a fork providing haskell-language-server:
, reflex-platform ? fetchGit {url = "https://github.com/ibizaman/reflex-platform.git"; ref = "haskell-language-server";}
, useWarp ? true
}:
(import reflex-platform {

  inherit system;
  hlsSupport = true;

  config.android_sdk.accept_license = true;

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

    android.frontend = {
      executableName = "hs-template-nix-reflexfrp";
      applicationId = "com.github.ibizaman.hstemplatenixreflexfrp";
      displayName = "HS Template Nix ReflexFRP";
    };

    ios.frontend = {
      executableName = "hs-template-nix-reflexfrp";
      bundleIdentifier = "com.github.ibizaman.hs-template-nix-reflexfrp";
      bundleName = "HS Template Nix ReflexFRP";
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

      mmorph = pkgs.haskell.lib.dontHaddock super.mmorph;
      x509-validation = pkgs.haskell.lib.dontCheck super.x509-validation; # Testing this takes just too much time
      tls = pkgs.haskell.lib.dontCheck super.tls; # Testing this takes just too much time
    };
  }
)

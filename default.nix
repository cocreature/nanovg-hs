{ pkgs ? import <nixpkgs> {} }:
let
  haskellPkgs = pkgs.haskell.packages.ghc843.override(oldAttrs: {
    overrides = self: super: {
      nanovg = super.callPackage ./nanovg.nix {};
    };
  });
in
haskellPkgs.nanovg

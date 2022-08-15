{ pkgs ? import <nixpkgs> {} }:
(pkgs.haskell.lib.addBuildTool
  (import ./default.nix { inherit pkgs; })
  pkgs.cabal-install
).env

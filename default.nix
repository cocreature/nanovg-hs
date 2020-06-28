{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.callCabal2nix "nanovg" ./. {
  GLEW = null;
  inherit (pkgs) glew;
  inherit (pkgs) libGL;
  inherit (pkgs) libGLU;
  inherit (pkgs.xorg) libX11;
}

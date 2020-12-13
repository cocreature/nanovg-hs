{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.callCabal2nixWithOptions "nanovg" ./. "-fexamples" {
  GLEW = null;
  inherit (pkgs) freetype;
  inherit (pkgs) glew;
  inherit (pkgs) libGL;
  inherit (pkgs) libGLU;
  inherit (pkgs.xorg) libX11;
}

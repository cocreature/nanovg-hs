{ mkDerivation, base, bytestring, c2hs, containers, glew, hspec
, inline-c, libGL, libGLU, QuickCheck, stdenv, text, vector
}:
mkDerivation {
  pname = "nanovg";
  version = "0.5.2.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base bytestring containers text vector ];
  librarySystemDepends = [ glew libGL libGLU ];
  libraryPkgconfigDepends = [ glew ];
  libraryToolDepends = [ c2hs ];
  testHaskellDepends = [ base containers hspec inline-c QuickCheck ];
  homepage = "https://github.com/cocreature/nanovg-hs";
  description = "Haskell bindings for nanovg";
  license = stdenv.lib.licenses.isc;
}

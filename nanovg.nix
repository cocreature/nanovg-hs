{ mkDerivation, base, bytestring, c2hs, containers, gl, glew
, GLFW-b, hspec, inline-c, libGL, libGLU, monad-loops, QuickCheck
, stdenv, text, transformers, vector
, lib
}:
mkDerivation {
  pname = "nanovg";
  version = "0.6.0.0";
  src = lib.cleanSourceWith {
    filter = path: type:
      let base = baseNameOf path;
      in !(lib.hasPrefix ".ghc.environment" base);
    src = ./.;
  };
  configureFlags = [ "-fexamples" ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base bytestring containers text vector ];
  librarySystemDepends = [ glew libGL libGLU ];
  libraryPkgconfigDepends = [ glew ];
  libraryToolDepends = [ c2hs ];
  executableHaskellDepends = [
    base containers gl GLFW-b monad-loops text transformers vector
  ];
  testHaskellDepends = [ base containers hspec inline-c QuickCheck ];
  homepage = "https://github.com/cocreature/nanovg-hs";
  description = "Haskell bindings for nanovg";
  license = stdenv.lib.licenses.isc;
}

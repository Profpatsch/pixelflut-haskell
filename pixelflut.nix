{ mkDerivation, base, bytestring, monad-loops, mtl, network, stdenv
}:
mkDerivation {
  pname = "pixelflut";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base bytestring mtl network ];
  testHaskellDepends = [ base monad-loops network ];
  homepage = "https://github.com/Profpatsch/pixelflut-haskell";
  description = "A small library for interfacing with Pixelflut";
  license = stdenv.lib.licenses.gpl3;
}

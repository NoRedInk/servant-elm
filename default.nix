{ mkDerivation, aeson, base, Diff, elm-export, hspec, HUnit, lens
, servant, servant-foreign, stdenv, text, wl-pprint-text
}:
mkDerivation {
  pname = "servant-elm";
  version = "0.4.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base elm-export lens servant servant-foreign text wl-pprint-text
  ];
  testHaskellDepends = [
    aeson base Diff elm-export hspec HUnit servant text
  ];
  homepage = "http://github.com/mattjbray/servant-elm#readme";
  description = "Automatically derive Elm functions to query servant webservices";
  license = stdenv.lib.licenses.bsd3;
}

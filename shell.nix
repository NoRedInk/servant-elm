{ pkgs ? import ./nix/pkgs.nix }:
let
  haskellPackages = pkgs.haskell.packages.stackage.lts-1226;
  project = haskellPackages.callPackage ./default.nix {};
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = project.env.nativeBuildInputs ++ [
    haskellPackages.cabal-install
    haskellPackages.stack
  ];
  GHC_ENVIRONMENT = "-";
}

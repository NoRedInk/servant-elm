let
  sources = import ./nix/sources.nix { };
  pkgs = import sources.nixpkgs { };
in pkgs.mkShell {
  buildInputs = [pkgs.stack];
  NIX_PATH = "nixpkgs=${pkgs.path}";
}

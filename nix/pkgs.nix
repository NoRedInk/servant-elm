import <nixpkgs> {
  overlays = [
    (import ./overlays/stackage-ltses)
  ];
}

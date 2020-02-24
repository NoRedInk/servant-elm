let
  src = builtins.fetchGit {
    url = "https://github.com/typeable/nixpkgs-stackage";
    rev = "6042df5e646d65b826add0a85d16304bee8e1dd5";
  };
in import src

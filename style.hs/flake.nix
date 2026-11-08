{

  inputs = {
    self.submodules = true;

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    flake-compat.url = "github:NixOS/flake-compat";
  };

  outputs = inputs@{ self, ... }:
    let nixpkgs = if inputs ? "nixpkgs" then inputs.nixpkgs else builtins.getFlake "nixpkgs";
        eachSystem = nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed;
        system-pkgs = system: nixpkgs.legacyPackages.${system};
    in {
      packages = eachSystem (system:
        let pkgs = system-pkgs system;
        in rec {
          default = fourmolu;
          fourmolu = import ./fourmolu.nix { inherit inputs system pkgs; };
          stylish-haskell = import ./stylish-haskell.nix { inherit inputs system pkgs; };
          hlint = import ./hlint.nix { inherit inputs system pkgs; };
        }
      );
    };

}

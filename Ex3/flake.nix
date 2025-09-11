{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        thisHaskell = pkgs.haskellPackages.override {
          overrides = self: super: {
            plleditor = self.callCabal2nix "PLLEditor" ./. { };
          };
        };
      in
      {
        packages.default = pkgs.haskell.lib.justStaticExecutables thisHaskell.plleditor;
        devShells.default = thisHaskell.shellFor {
          packages = p: [ p.plleditor ];

          withHoogle = false;

          buildInputs = [
            thisHaskell.cabal-install
            thisHaskell.haskell-language-server
          ];
        };
      }
    );
}

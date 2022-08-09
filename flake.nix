{
  description = "counton";

  inputs =
    {
      nixpkgs.url = "github:NixOS/nixpkgs/22.05";
      flake-utils.url = "github:numtide/flake-utils";
      vector-hashtables = {
        url = "github:klapaucius/vector-hashtables";
        flake = false;
      };
      clutter = {
        url = "github:noughtmare/clutter";
        flake = false;
      };
      compact = {
        url = "github:ezyang/compact";
        flake = false;
      };
      text = {
        url = "github:haskell/text";
        flake = false;
      };
      with-utf8 = {
        url = "github:serokell/haskell-with-utf8";
        flake = false;
      };
      http-client = {
        url = "github:snoyberg/http-client";
        flake = false;
      };
    };

  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem (flake-utils.lib.defaultSystems) (system:
      let
        pkgs = (import nixpkgs { inherit system; });
        haskellPackages = pkgs.haskell.packages.ghc8107;
        project = returnShellEnv:
          haskellPackages.developPackage {
            inherit returnShellEnv;
            root = self;
            name = "counton";
            source-overrides = {
              inherit (inputs) vector-hashtables;
            };
            overrides = hself: hsuper: {
              mkDeriviation = args: hsuper.mkDeriviation (args // { enableLibraryProfiling = true; doCheck = false; });
              vector-hashtables = pkgs.haskell.lib.dontCheck hsuper.vector-hashtables;
            };
            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv
                (with haskellPackages; [
                  cabal-install
                  ghcid
                  haskell-language-server
                  fourmolu
                  hp2pretty
                  hs-speedscope
                  hlint
                ]);
          };
      in
      {
        defaultPackage = project false;
        devShell = project true;
      });
}

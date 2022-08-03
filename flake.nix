{
  description = "counton";

  inputs =
    {
      nixpkgs.url = "github:NixOS/nixpkgs/22.05";
      flake-utils.url = "github:numtide/flake-utils";
      vector-hashtables = {
        url = "https://hackage.haskell.org/package/vector-hashtables-0.1.1.1/vector-hashtables-0.1.1.1.tar.gz";
        flake = false;
      };
    };

  outputs = { self, nixpkgs, flake-utils, vector-hashtables }:
    flake-utils.lib.eachSystem (flake-utils.lib.defaultSystems) (system:
      let
        pkgs = (import nixpkgs { inherit system; });
        haskellPackages = pkgs.haskell.packages.ghc922;
        project = returnShellEnv:
          haskellPackages.developPackage {
            inherit returnShellEnv;
            root = self;
            name = "counton";
            source-overrides = {
              vector-hashtables = vector-hashtables;
            };
            overrides = hself: hsuper: {
              mkDeriviation = args: hsuper.mkDeriviation (args // { enableLibraryProfiling = true; });
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
                  hpack
                ]);
          };
      in
      {
        defaultPackage = project false;
        devShell = project true;
      });
}

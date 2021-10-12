{
  description = "JReduce - A jvm class-file input reducer";

  inputs = 
    {
      nixpkgs.url = "github:NixOS/nixpkgs/21.05";
      flake-utils.url = "github:numtide/flake-utils";
      recursion-schemes = { 
        url = "https://hackage.haskell.org/package/recursion-schemes-5.2.2.1/recursion-schemes-5.2.2.1.tar.gz";
        flake = false;
      };
      hspec-hedgehog = { 
        url = "https://hackage.haskell.org/package/hspec-hedgehog-0.0.1.2/hspec-hedgehog-0.0.1.2.tar.gz";
        flake = false;
      };
      jvm-binary = { 
        url = "https://hackage.haskell.org/package/jvm-binary-0.10.0/jvm-binary-0.10.0.tar.gz";
        flake = false;
      };
      reduce-src = {
        url = "github:ucla-pls/reduce";
        flake = false;
      };
      jvmhs = { 
        url = "github:ucla-pls/jvmhs";
        flake = false;
      };
    };

  outputs = { self, nixpkgs, recursion-schemes, flake-utils, reduce-src
    , hspec-hedgehog, jvmhs, jvm-binary }:
    flake-utils.lib.eachSystem [ "x86_64-darwin" ] (system:
      let
        pkgs = (import nixpkgs { inherit system; });
        haskellPackages = pkgs.haskellPackages;
        project = returnShellEnv:
          haskellPackages.developPackage {
            inherit returnShellEnv;
            root = self;
            name = "jreduce";
            source-overrides = {
              reduce = "${reduce-src}/reduce";
              reduce-util = "${reduce-src}/reduce-util";
              inherit hspec-hedgehog jvmhs jvm-binary;
            };
            overrides = hsuper: hself: { };
            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv
              (with haskellPackages; [ cabal-install ghcid ]);
          };
      in {
        defaultPackage = project false;
        devShell = project true;
      });
}

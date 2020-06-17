{ compiler ? "default"
, pkgs ? fix.nixpkgs {}

, overrides ? {}
, fix-src ? ./nix/fix
, fix ? import fix-src fix // overrides
}:
let
  haskellPackages =
    if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages."${compiler}";
in
  haskellPackages.developPackage {
    root = pkgs.lib.cleanSourceWith
      { filter = path: type: baseNameOf path != ".nix";
        src = pkgs.lib.cleanSource ./.;
      };
    name = "jreduce";
    source-overrides = {
      inherit (fix) reduce reduce-util hspec-hedgehog jvmhs dirtree jvm-binary;
    };
    overrides = hsuper: hself: { };
    modifier = drv:
      with pkgs.haskell.lib;
      addBuildTools drv (with haskellPackages; [ cabal-install ghcid ])
    ;
  }

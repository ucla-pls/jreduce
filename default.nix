{ pkgs ? import ./nix/nixpkgs.nix {}
, compiler ? "default"
, jvmhs ? import ./nix/jvmhs.nix 
, jvm-binary ? import ./nix/jvm-binary.nix 
, reduce-util ? "${import ./nix/reduce.nix}/reduce-util"
, reduce ? "${import ./nix/reduce.nix}/reduce"
, dirtree ? "${import ./nix/dirtree.nix}"
}: 
let 
  haskellPackages = 
    if compiler == "default" 
    then pkgs.haskellPackages 
    else pkgs.haskell.packages."${compiler}";
in
  haskellPackages.developPackage {
    root = builtins.filterSource 
      (path: type: baseNameOf path != ".nix")
      ./.;
    name = "jreduce";
    source-overrides = {
      inherit jvmhs jvm-binary reduce-util reduce dirtree;
    };
    overrides = hsuper: hself: { };
    modifier = drv:
      with pkgs.haskell.lib;
      addBuildTools drv (with haskellPackages; [ cabal-install ghcid ])
    ;
  }

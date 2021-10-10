{ compiler ? "default"
, pkgs ? fix.nixpkgs {}

, overrides ? {}
, fix-src ? ./fix
, fix ? import fix-src fix // overrides
}:
let
  haskellPackages =
    if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages."${compiler}";
in
  pkgs.mkShell {
    name = "with-jreduce";
    nativeBuildInputs = [ 
      pkgs.openjdk8
      ( with pkgs.haskell.lib;
      dontHaddock (dontCheck (disableLibraryProfiling
        ((haskellPackages.extend (pkgs.haskell.lib.packageSourceOverrides {
          jreduce = ./..;
          inherit (fix) reduce reduce-util hspec-hedgehog jvmhs jvm-binary;
       })).jreduce)
       )))
    ];
  }

self:
{
  reduce-src = import ./reduce.nix;
  reduce = "${self.reduce-src}/reduce";
  reduce-util = "${self.reduce-src}/reduce-util";
  hspec-hedgehog = import ./hspec-hedgehog.nix;
  nixpkgs = import ./nixpkgs.nix;
  jvmhs = import ./jvmhs.nix;
  jvm-binary = import ./jvm-binary.nix;
  dirtree = import ./dirtree.nix;
}

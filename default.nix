{ mkDerivation, aeson, base, bytestring, cassava, containers
, deepseq, directory, dirtree, filepath, hpack, jvm-binary, jvmhs
, lens, lens-action, mtl, optparse-applicative, reduce, reduce-util
, stdenv, text, time, transformers, unliftio, unordered-containers
, vector, zip-archive
}:
mkDerivation {
  pname = "jreduce";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring cassava containers deepseq directory dirtree
    filepath jvm-binary jvmhs lens lens-action mtl optparse-applicative
    reduce reduce-util text time transformers unliftio
    unordered-containers vector zip-archive
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring cassava containers deepseq directory dirtree
    filepath jvm-binary jvmhs lens lens-action mtl optparse-applicative
    reduce reduce-util text time transformers unliftio
    unordered-containers vector zip-archive
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/ucla-pls/jvmhs#readme";
  description = "A tool for reducing Java Bytecode files";
  license = stdenv.lib.licenses.bsd3;
}

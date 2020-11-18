{ mkDerivation, base, containers, directory, filepath
, optparse-applicative, optparse-text, protolude, random, stdenv
, svg-builder, text
}:
mkDerivation {
  pname = "svg";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers directory filepath optparse-applicative
    optparse-text protolude random svg-builder text
  ];
  homepage = "https://github.com/janeymunoz/svg.git";
  description = "Command line tool for generating SVGs";
  license = stdenv.lib.licenses.bsd3;
}

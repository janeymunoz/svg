{ mkDerivation, base, directory, filepath, optparse-applicative
, protolude, stdenv, svg-builder, text
}:
mkDerivation {
  pname = "svg";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base directory filepath optparse-applicative protolude svg-builder
    text
  ];
  homepage = "https://github.com/janeymunoz/svg.git";
  description = "Command line tool for generating SVGs";
  license = stdenv.lib.licenses.bsd3;
}

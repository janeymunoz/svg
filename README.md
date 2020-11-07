# svg

A command line tool to generate
[SVGs](https://developer.mozilla.org/en-US/docs/Web/SVG).

## Development

This project uses [Nix](https://nixos.org/) to manage dependencies and build the
executable.

Updating dependencies: `cabal2nix . > default.nix`

Building: `nix-build release.nix`

Developing: `nix-shell --command ghcid`

Creating the `nix-shell` file: `cabal2nix . --shell > shell.nix`

## Usage

A successful build of the project will output a binary file in the `/nix/store/`
path. The specific path should be shown in the output, after the build command
was invoked. Executing the program with a flag of `--help` will output details
on what arguments may be passed to generate a semi-custom SVG.

For example:

```
$ /nix/store/ryqx2vsvrxxs7d9xpyjaiq6y52snc3dn-svg-0.1.0.0/bin/svg --help
An SVG generator

Usage: svg [-1|--line1 ARG] [-2|--line2 ARG] [-3|--line3 ARG] [-4|--line4 ARG] 
           [-5|--line5 ARG]
  -h,--help                Show this help text
...
```

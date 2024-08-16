{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:
let pack = import ./default.nix { pkgs = nixpkgs; };
in nixpkgs.haskellPackages.shellFor {
  packages = p: [ pack ];
  withHoogle = true;
  buildInputs = with nixpkgs; [ cabal-install haskell-language-server stylish-haskell];
}

{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  packages = [
    (pkgs.ghc.withPackages (hs: [hs.random]))
  ];
}

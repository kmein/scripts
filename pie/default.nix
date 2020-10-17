{ pkgs ? import <nixpkgs> {} }:
with pkgs.haskellPackages;
developPackage {
  root = ./.;
  modifier = drv: pkgs.haskell.lib.addBuildTools drv [ cabal-install ghcid ];
}

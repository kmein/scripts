{ pkgs ? import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/d2caa9377539e3b5ff1272ac3aa2d15f3081069f.tar.gz") {} }:
with pkgs;
haskellPackages.developPackage {
  root = ./.;
  modifier = drv: haskell.lib.addBuildTools drv (with haskellPackages; [
    cabal-install
    ghcid
    (hoogleLocal { packages = drv.propagatedBuildInputs; })
    fourmolu
  ]);
}

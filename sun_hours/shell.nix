{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = [
    pkgs.python3Packages.astral
    pkgs.python3Packages.matplotlib
    pkgs.python3Packages.click
    pkgs.python3Packages.timezonefinder
  ];
}

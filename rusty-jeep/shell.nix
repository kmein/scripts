{pkgs ? import <nixpkgs> {}}:
pkgs.mkShell {
  packages = [
    pkgs.rustc
    pkgs.cargo
    pkgs.rustfmt
    pkgs.pkg-config
    pkgs.alsa-lib
  ];
}

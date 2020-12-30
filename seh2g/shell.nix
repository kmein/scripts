{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = with pkgs; [
    pkgs.libxml2 # for xmllint
    (pkgs.writers.writeDashBin "serve" ''
      ${pkgs.python3}/bin/python3 -m http.server
    '')
  ];
}

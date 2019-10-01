{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = with pkgs; [
    pandoc
    python3Packages.beautifulsoup4
    python3Packages.requests
    python3Packages.lxml
  ];
  shellHook = "export HISTFILE=${toString ./.history}";
}

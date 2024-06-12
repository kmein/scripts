{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  packages = [
    (pkgs.python3.withPackages (py: [
      py.jupyter
      py.numpy
      py.pandas
      py.matplotlib
      py.scikit-learn
      py.plotly
    ]))
  ];
}

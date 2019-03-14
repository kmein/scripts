with import <nixpkgs> {};
stdenv.mkDerivation {
    name = "bvg-node";
    buildInputs = [ nodePackages.yarn nodePackages.typescript ];
    shellHook = ''
        export PATH=$PATH:$(pwd)/node_modules/.bin
    '';
}
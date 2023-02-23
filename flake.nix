{
  description = "All kinds of stuff";

  inputs = {
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.follows = "rust-overlay/flake-utils";
    nixpkgs.follows = "rust-overlay/nixpkgs";
  };

  outputs = inputs@{nixpkgs, flake-utils, ...}:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          inputs.rust-overlay.overlays.default
        ];
      };
    in
    {
      packages.onomap = pkgs.haskellPackages.callCabal2nix "onomap" ./onomastics-ng {};
      packages.rusty-jeep = pkgs.rustPlatform.buildRustPackage rec {
        name = "rusty-jeep";
        version = "1.0.0";
        src = ./rusty-jeep;
        nativeBuildInputs = [pkgs.pkg-config];
        buildInputs = [pkgs.alsa-lib];
        cargoHash = "sha256-3Zuh22qGSIhyOnwoZqpf3eio5l2q9u6XTUpiCBnAHrA=";
        meta = with nixpkgs.lib; {
          description = "A beeping program inspired by K_belwagen";
          license = licenses.wtfpl;
          maintainers = [maintainers.kmein];
        };
      };
    });
}

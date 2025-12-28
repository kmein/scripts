{
  description = "All kinds of stuff";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs =
    {
      self,
      nixpkgs,
      rust-overlay,
    }:
    let
      pkgsFor =
        system:
        import nixpkgs {
          inherit system;
          overlays = [
            rust-overlay.overlays.default
          ];
        };
      lib = nixpkgs.lib;
      eachSupportedSystem = lib.genAttrs lib.systems.flakeExposed;
    in
    {
      packages = eachSupportedSystem (
        system:
        let
          pkgs = pkgsFor system;
        in
        {
          bvg =
            let
              env = pkgs.bundlerEnv {
                name = "bvg-env";
                ruby = pkgs.ruby;
                gemfile = bvg/Gemfile;
                lockfile = bvg/Gemfile.lock;
                gemset = bvg/gemset.nix;
              };
            in
            pkgs.stdenv.mkDerivation {
              name = "bvg";
              buildInput = [ env.wrappedRuby ];
              script = bvg/bvg.rb;
              buildCommand = ''
                install -D -m755 $script $out/bin/bvg
                patchShebangs $out/bin/bvg
              '';
            };
          onomap = pkgs.haskellPackages.callCabal2nix "onomap" ./onomastics-ng { };
          hesychius = hesychius/hesychius.txt;
          pun-sort-api = pkgs.callPackage ./pun-sort {};
          alarm = pkgs.writers.writeDashBin "alarm" ''
            set -efu
            export PATH=${
              nixpkgs.lib.makeBinPath [
                pkgs.coreutils
                pkgs.bc
                self.packages.${system}.rusty-jeep
              ]
            }
            for i in `seq 8000 1000 10000`; do
            echo $i 100
            done | rusty-jeep
            echo 'if you heard that sound, then goto sleep..^_^'

            echo sleep "$@"
            sleep "$@"

            echo 'wake up!'
            while :; do
            echo $(echo "($(od -tu -An -N 2 /dev/urandom)%1000)+500"|bc) $(echo "($(od -tu -An -N 2 /dev/urandom)%500)+100"|bc)
            done | rusty-jeep 1
          '';
          rusty-jeep = pkgs.rustPlatform.buildRustPackage rec {
            name = "rusty-jeep";
            version = "1.0.0";
            src = ./rusty-jeep;
            nativeBuildInputs = [ pkgs.pkg-config ];
            buildInputs = [ pkgs.alsa-lib ];
            cargoHash = "sha256-L3lpWEb2y9+45jWnpWNBsYU4nNDx0reSxjw1eKYb+6A=";
            meta = with nixpkgs.lib; {
              description = "A beeping program inspired by K_belwagen";
              license = licenses.wtfpl;
              maintainers = [ maintainers.kmein ];
            };
          };
        }
      );
    };
}

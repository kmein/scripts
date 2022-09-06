{ writers, haskellPackages, ... }:
writers.writeHaskellBin "web-socket-sink" {
  libraries = with haskellPackages; [websockets text concurrency unagi-chan optparse-applicative];
} (builtins.readFile ./Server.hs)

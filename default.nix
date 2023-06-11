{ pkgs }:
let
  localHaskellPkgs = pkgs.haskell.packages.ghc927;
in
  localHaskellPkgs.callCabal2nix "hm-type-system" ./. {}

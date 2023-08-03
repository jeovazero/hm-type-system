{ haskellPkgs }:
  haskellPkgs.callCabal2nix "hm-type-system" ./. {}

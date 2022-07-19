{
  description = "A very basic flake";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          nexus-mods =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc902";
              shell.tools = {
                cabal = {};
                haskell-language-server = {};
                hlint = {};
                fourmolu = {};
              };
              shell.buildInputs = with pkgs; [
                zlib
              ];
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.nexus-mods.flake {};
    in flake // {
      defaultPackage = flake.packages."nexus-mods:lib:nexus-mods";
    });
}

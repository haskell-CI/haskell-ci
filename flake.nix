{
  description = "Scripts for setting up CI for Haskell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  nixConfig.allow-import-from-derivation = true; # cabal2nix uses IFD

  outputs = { self, nixpkgs, flake-utils }:
    let
      ghcVer = "ghc902";
      makeHaskellOverlay = overlay: final: prev: {
        haskell = prev.haskell // {
          packages = prev.haskell.packages // {
            ${ghcVer} = prev.haskell.packages."${ghcVer}".override (oldArgs: {
              overrides =
                prev.lib.composeExtensions (oldArgs.overrides or (_: _: { }))
                  (overlay prev);
            });
          };
        };
      };

      out = system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ self.overlays.default ];
          };

        in
        {
          packages = rec {
            default = haskell-ci;
            haskell-ci = pkgs.haskell.lib.justStaticExecutables pkgs.haskell.packages.${ghcVer}.haskell-ci;
          };

          checks = {
            inherit (self.packages.${system}) haskell-ci;
          };

          # for debugging
          inherit pkgs;

          devShells.default =
            let haskellPackages = pkgs.haskell.packages.${ghcVer};
            in
            haskellPackages.shellFor {
              packages = p: [ self.packages.${system}.haskell-ci ];
              withHoogle = true;
              buildInputs = with haskellPackages; [
                # just commented out because they take extra time to build
                # given the overrides below:
                #
                # haskell-language-server
                # fourmolu

                # broken on this nixpkgs version
                # ghcid
                cabal-install
              ] ++ (with pkgs; [
                sqlite
              ]);
              # Change the prompt to show that you are in a devShell
              # shellHook = "export PS1='\\e[1;34mdev > \\e[0m'";
            };
        };
    in
    flake-utils.lib.eachDefaultSystem out // {
      # this stuff is *not* per-system
      overlays = {
        default = makeHaskellOverlay (prev: hfinal: hprev:
          let
            hlib = prev.haskell.lib;
            # filter paths so that readme changes don't force rebuilds
            filteredSource = prev.lib.sourceByRegex ./. [
              "^.*\\.hs$"
              "^.*\\.cabal$"

              "LICENSE"
              "CHANGELOG\\.md"

              "^src.*$"
              "^cli.*$"
              "^test.*$"
              "^fixtures.*$"
            ];
          in
          {
            haskell-ci = hprev.callCabal2nix "haskell-ci" filteredSource {
              # this has to be overridden like this because it is looking
              # villainy directly in the face to override Cabal
              Cabal-syntax = hfinal.Cabal-syntax_3_8_1_0;
            };

            # these are just old in the default nixpkgs configuration
            base-compat = hfinal.base-compat_0_12_2;
            base-compat-batteries = hfinal.base-compat-batteries_0_12_2;
            optparse-applicative = hfinal.optparse-applicative_0_17_0_0;
          });
      };
    };
}

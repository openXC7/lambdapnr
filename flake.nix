{
  description = "lambdapnr: Haskell FPGA place-and-route — development shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);
    in
    {
      devShells = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
          # GHC + HLS from the same nixpkgs snapshot -> guaranteed compatible
          ghc = pkgs.haskellPackages.ghc;
        in
        {
          default = pkgs.mkShell {
            name = "lambdapnr-dev";

            packages = [
              # toolchain
              ghc
              pkgs.cabal-install
              pkgs.haskell-language-server

              # formatting / linting
              pkgs.fourmolu
              pkgs.hlint
              pkgs.haskellPackages.cabal-fmt

              # build / dev loop
              pkgs.haskellPackages.cabal-plan
              pkgs.ghcid
              pkgs.haskellPackages.hoogle

              # performance analysis (SPECIFICATION.md section 7.4)
              pkgs.haskellPackages.ghc-prof-flamegraph
              pkgs.haskellPackages.eventlog2html
            ];

            shellHook = ''
              echo "lambdapnr dev shell"
              echo "  ghc:    $(ghc --version)"
              echo "  cabal:  $(cabal --version | head -1)"
              echo "  hls:    $(haskell-language-server-wrapper --version 2>/dev/null | head -1 || true)"
              echo "  hoogle: $(hoogle --version 2>/dev/null | head -1 || true)"
              echo "quick check:  cabal build all"
              echo "quick tests:  cabal test"
              echo "watch tests:  ghcid --command 'cabal repl test:lambdapnr-test'"
            '';
          };
        });
    };
}

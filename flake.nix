{
  description = "NCurses support for Idris 2 apps compiled with the Chez Scheme backend.";

  inputs = {
    idris2-packageset.url = "github:mattpolzin/nix-idris2-packages";
  };

  outputs =
    {
      self,
      nixpkgs,
      idris2-packageset,
      ...
    }:
    let
      inherit (nixpkgs) lib;
      forEachSystem =
        f: lib.genAttrs lib.systems.flakeExposed (system: f system nixpkgs.legacyPackages.${system});
    in
    {
      packages = forEachSystem (
        system: pkgs:
        let
          buildIdris' = idris2-packageset.buildIdris'.${system};
        in
        {
          default = buildIdris' {
            ipkgName = "ncurses-idris";
            src = builtins.path {
              path = ./.;
              name = "ncurses-idris-src";
            };
            buildInputs = [
              pkgs.ncurses5
            ];
          };
        }
      );
      devShells = forEachSystem (
        system: pkgs:
        let
          inherit (idris2-packageset.packages.${system}) idris2 idris2Lsp;
          inherit (nixpkgs.legacyPackages.${system}) mkShell;
        in
        {
          default = mkShell {
            packages = [
              idris2
              idris2Lsp
            ];

            inputsFrom = [
              self.packages.${system}.default
            ];
          };
        }
      );
      formatter = forEachSystem (system: pkgs: pkgs.nixfmt-rfc-style);

      checks = forEachSystem (
        system: pkgs: {
          control_curses_ticker_example = import ./examples/control_curses_ticker {
            buildIdris = idris2-packageset.buildIdris.${system};
            ncurses-idris = self.packages.${system}.default;
          };
        }
      );
    };
}

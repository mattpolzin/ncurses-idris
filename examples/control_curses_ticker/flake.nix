{
  description = "An example NCurses app (with a ticker) flake";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    ncurses-idris.url = "github:mattpolzin/ncurses-idris/main";
  };

  outputs = { nixpkgs, flake-utils, ncurses-idris, ... }:
  flake-utils.lib.eachDefaultSystem (system:
    let pkgs = nixpkgs.legacyPackages.${system};
        ncurses-idris' = ncurses-idris.packages.${system}.default;
    in rec {
      packages.control-curses-ticker = import ./. { inherit (pkgs) buildIdris; ncurses-idris = ncurses-idris'; };
      packages.default = packages.control-curses-ticker;
      apps.control-curses-ticker = flake-utils.lib.mkApp { drv = packages.control-curses-ticker; };
      apps.default = apps.control-curses-ticker;
    }
  );
}

{
  description = "An example NCurses app (with a ticker) flake";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    idris2 = {
      url = "github:idris-lang/idris2/main";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    ncurses-idris = {
      url = "github:mattpolzin/ncurses-idris/nix-experiment";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.idris2.follows = "idris2";
    };
  };

  outputs = { self, nixpkgs, flake-utils, idris2, ncurses-idris }:
  flake-utils.lib.eachDefaultSystem (system:
    let pkgs = nixpkgs.legacyPackages.${system};
        inherit (pkgs) stdenv;
        idris2' = idris2.defaultPackage.${system};
        ncurses-idris' = ncurses-idris.packages.${system}.default;
        inherit (builtins) concatStringsSep;
    in rec {
      packages.control-curses-ticker = stdenv.mkDerivation rec {
        name = "control-curses-ticker";
        version = "0.0.1";
        src = ./.;
        idrisPackages = [ ncurses-idris' ] ++ ncurses-idris'.idrisPackages;
        buildInputs = [
          idris2'
        ] ++ idrisPackages;

        IDRIS2="${idris2'}/bin/idris2";
        IDRIS2_PACKAGE_PATH = concatStringsSep ":" (map (p: "${p}/idris2-${idris2'.version}") idrisPackages);

        buildPhase = ''
          make clean
          make app
        '';

        installPhase = ''
          rm ./build/exec/control_curses_ticker_app/compileChez
          rm ./build/exec/control_curses_ticker_app/control_curses_ticker.ss
          mkdir -p $out/bin/control_curses_ticker_app
          install ./build/exec/control_curses_ticker_app/* $out/bin/control_curses_ticker_app/
          install ./build/exec/control_curses_ticker $out/bin/
        '';
      };
      packages.default = packages.control-curses-ticker;
      apps.control-curses-ticker = flake-utils.lib.mkApp { drv = packages.control-curses-ticker; };
      apps.default = apps.control-curses-ticker;
    }
  );
}

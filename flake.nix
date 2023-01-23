{
  description = "NCurses support for Idris 2 apps compiled with the Chez Scheme backend.";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    idris2 = {
      url = "github:idris-lang/Idris2/main";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    idris-indexed = {
      url = "github:mattpolzin/idris-indexed/nix-experiment";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.idris2.follows = "idris2";
    };
  };

  outputs = { self, nixpkgs, flake-utils, flake-compat, idris2, idris-indexed }:
    flake-utils.lib.eachDefaultSystem (system: 
      let pkgs = nixpkgs.legacyPackages.${system};
          stdenv = pkgs.stdenv;
          ncurses = pkgs.ncurses;
          lists = pkgs.lib.lists;
          idris2' = idris2.defaultPackage.${system};
          idris-indexed' = idris-indexed.packages.${system}.default;
      in rec {
      packages.default = stdenv.mkDerivation rec {
        name = "ncurses-idris";
        version = "0.4.0";
        src = ./.;
        idrisPackages = [ idris-indexed' ] ++ idris-indexed'.idrisPackages;
        propagatedBuildInputs = [
          ncurses
        ];
        buildInputs = [
          idris2'
        ] ++ idrisPackages;

        IDRIS2 = "${idris2'}/bin/idris2";
        IDRIS2_PREFIX = "${placeholder "out"}";
        INDEXED_INSTALL_LOCATION = "${idris-indexed'}/idris2-0.6.0";
        IDRIS2_PACKAGE_PATH = builtins.concatStringsSep ":" (builtins.map (p: "${p}/idris2-${idris2'.version}") idrisPackages);

        buildPhase = ''
          make clean
          make all
        '';
        installPhase = ''
          make install
        '';
      };
      checks.control_curses_ticker_example = 
        (import flake-compat { src = ./examples/control_curses_ticker; inherit system; }).defaultNix.default;
    }
  );
}

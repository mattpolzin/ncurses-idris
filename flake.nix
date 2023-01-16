{
  description = "NCurses support for Idris 2 apps compiled with the Chez Scheme backend.";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    idris2 = {
      url = "github:idris-lang/idris2/main";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    idris-indexed = {
      url = "github:mattpolzin/idris-indexed/nix-experiment";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.idris2.follows = "idris2";
    };
  };

  outputs = { self, nixpkgs, flake-utils, idris2, idris-indexed }:
    flake-utils.lib.eachDefaultSystem (system: 
      let pkgs = nixpkgs.legacyPackages.${system};
          stdenv = pkgs.stdenv;
          ncurses = pkgs.ncurses;
          idris2' = idris2.defaultPackage.${system};
          idris-indexed' = idris-indexed.packages.${system}.default;
      in {
      packages.default = stdenv.mkDerivation rec {
        name = "ncurses-idris";
        version = "0.4.0";
        src = ./.;
        propagatedBuildInputs = [
          ncurses

          idris-indexed'
        ];
        buildInputs = [
          idris2'
        ];

        IDRIS2="${idris2'}/bin/idris2";
        INDEXED_INSTALL_LOCATION="${idris-indexed'}/idris2-0.6.0";

        buildPhase = ''
          export IDRIS2_PACKAGE_PATH="$IDRIS2_PACKAGE_PATH:${INDEXED_INSTALL_LOCATION}";
          make clean
          make all
        '';
        installPhase = ''
          export IDRIS2_PREFIX="$out"
          make install
        '';
      };
    }
  );
}

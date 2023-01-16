{
  description = "An example NCurses app (with a ticker) flake";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    idris2 = {
      url = "github:idris-lang/idris2/main";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    ncurses-idris = {
      url = "github:mattpolzin/ncurses-idris/nix-experiment";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.idris2.follows = "idris2";
    };
  };

  outputs = { self, nixpkgs }: {

  };
}

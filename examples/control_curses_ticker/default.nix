{ buildIdris, ncurses-idris }:
(buildIdris {
  ipkgName = "control_curses_ticker";
  version = "0.0.1";
  src = ./.;
  idrisLibraries = [ ncurses-idris ];
}).executable

# ncurses-idris
A simple very incomplete package for interfacing with ncurses from Idris 2 using the c `libncurses` library.

## Dependencies
Packages:
- `contrib >= 0.5.1`
- [`indexed >= 0.0.5`](http://mattpolzin.com/idris-indexed.git)

System libraries:
`ncurses >= 5`

## Building
To install this package, first install the `indexed` package (or put it in the `depends` directory of this repo) and then run
```shell
make && make install
```

On some systems, targetting `libncurses` generically will fail (you'll know when you try to run a program using this Idris package) so you need to build this package for a specific version of `libncurses`.

Determining what version you have is system dependent, but once you know, you can install this package for a particular version as follows (version `6` in this example):
```shell
NCURSES_VERSION=6 make && make install
```

## Use
This library currently supports relatively low-level access to some but not all NCurses features and
somewhat more limited support of the same features via a dependently-typed wrapper. See modules under
`NCurses` for more on low-level access in conjunction with the `Text.PrettyPrint.Prettyprinter.Render.NCurses`
module provided by this package to render `Doc`s with `NCurses.Core.Attribute` annotations.

The higher level access is provided by the `Control.NCurses` module and it also supports pretty-printing of
`Doc`s via `Control.NCurses.Pretty`.

You can find an example of low-level use (including pretty-printing of `Doc`s) in `examples/doc_ann`.

You can find an example of high-level use (including pretty-printing of `Doc`s) in `examples/control_curses`.

When using low-level access, be sure you `initNCurses` before doing anything else and be sure you cleanup
with `deinitNCurses` before your program exits.

The following fork by @gallais may pique the interest of anyone looking to experiment with a more Idris-like interface to the underlying NCurses functionality: https://github.com/gallais/ncurses-idris

# ncurses-idris
A simple very incomplete package for interfacing with ncurses from Idris 2 using the c `libncurses` library.

This library currently supports relatively low-level access to some but not all NCurses features. See
modules under `NCurses` for more on that. You can also use the `Text.PrettyPrint.Prettyprinter.Render.NCurses`
module provided by this package to render `Doc`s with `NCurses.Core.Attribute` annotations.

Be sure you `initNCurses` before doing anything else and be sure you cleanup with `deinitNCurses` before your program exits.

The following fork by @gallais may pique the interest of anyone looking to experiment with a more Idris-like interface to the underlying NCurses functionality: https://github.com/gallais/ncurses-idris

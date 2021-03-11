# ncurses-idris
A simple very incomplete package for interfacing with ncurses from Idris 2 using the c `libncurses` library.

See `NCurses.idr` to find out what ncurses features are supported.

Be sure you `initNCurses` before doing anything else and be sure you cleanup with `deinitNCurses` before your program exits.

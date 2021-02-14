#ifndef _NCURSES_IDRIS_HELPERS_H
#define _NCURSES_IDRIS_HELPERS_H

#include<ncurses.h>

WINDOW* std_win();

int normal_attr();
int underline_attr();
int standout_attr();

#endif

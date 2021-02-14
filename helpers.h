#ifndef _NCURSES_IDRIS_HELPERS_H
#define _NCURSES_IDRIS_HELPERS_H

#include<ncurses.h>

WINDOW* std_win();

int normal_attr();
int underline_attr();
int standout_attr();
int reverse_attr();
int blink_attr();
int dim_attr();
int bold_attr();
int protected_attr();
int invisible_attr();
int color_pair_attr(int idx);

#endif

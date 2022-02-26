#ifndef _NCURSES_IDRIS_HELPERS_H
#define _NCURSES_IDRIS_HELPERS_H

#include<ncurses.h>

WINDOW* std_win();

// The error return value for all int-returning functions.
int ERR();

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
int black_color();
int red_color();
int green_color();
int yellow_color();
int blue_color();
int magenta_color();
int cyan_color();
int white_color();

int keyF0();
int keyF1();
int keyF2();
int keyF3();
int keyF4();
int keyF5();
int keyF6();
int keyF7();
int keyF8();
int keyF9();
int keyF10();
int keyF11();
int keyF12();
int keyUp();
int keyDown();
int keyLeft();
int keyRight();
int keyBackspace();

#endif

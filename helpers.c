
#include "helpers.h"
#include<ncurses.h>

WINDOW* std_win() {
  return stdscr;
}

int normal_attr() {
  return A_NORMAL;
}

int underline_attr() {
  return A_UNDERLINE;
}

int standout_attr() {
  return A_STANDOUT;
}



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

int reverse_attr() {
  return A_REVERSE;
}

int blink_attr() {
  return A_BLINK;
}

int dim_attr() {
  return A_DIM;
}

int bold_attr() {
  return A_BOLD;
}

int protected_attr() {
  return A_PROTECT;
}

int invisible_attr() {
  return A_INVIS;
}

int color_pair_attr(int idx) {
  return COLOR_PAIR(idx);
}


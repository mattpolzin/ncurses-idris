
#include "curses-helpers.h"
#include<ncurses.h>

WINDOW* std_win() {
  return stdscr;
}

int ncurses_err() {
  return ERR;
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

int black_color() {
  return COLOR_BLACK;
}

int red_color() {
  return COLOR_RED;
}

int green_color() {
  return COLOR_GREEN;
}

int yellow_color() {
  return COLOR_YELLOW;
}

int blue_color() {
  return COLOR_BLUE;
}

int magenta_color() {
  return COLOR_MAGENTA;
}

int cyan_color() {
  return COLOR_CYAN;
}

int white_color() {
  return COLOR_WHITE;
}

int keyF0() {
  return KEY_F0;
}

int keyF1() {
  return KEY_F(1);
}

int keyF2() {
  return KEY_F(2);
}

int keyF3() {
  return KEY_F(3);
}

int keyF4() {
  return KEY_F(4);
}

int keyF5() {
  return KEY_F(5);
}

int keyF6() {
  return KEY_F(6);
}

int keyF7() {
  return KEY_F(7);
}

int keyF8() {
  return KEY_F(8);
}

int keyF9() {
  return KEY_F(9);
}

int keyF10() {
  return KEY_F(10);
}

int keyF11() {
  return KEY_F(11);
}

int keyF12() {
  return KEY_F(12);
}

int keyUp() {
  return KEY_UP;
}

int keyDown() {
  return KEY_DOWN;
}

int keyLeft() {
  return KEY_LEFT;
}

int keyRight() {
  return KEY_RIGHT;
}

int keyBackspace() {
  return KEY_BACKSPACE;
}

int print(const char* fmt, const char* arg) {
  return printw(fmt, arg);
}

int printWindow(WINDOW* win, const char* fmt, const char* arg) {
  return wprintw(win, fmt, arg);
}

int mvPrint(int x, int y, const char* fmt, char* arg) {
  return mvprintw(x, y, fmt, arg);
}

int mvPrintWindow(WINDOW* win, int x, int y, const char* fmt, char* arg) {
  return mvwprintw(win, x, y, fmt, arg); 
}

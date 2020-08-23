#include "utility.h"

void CenterWindow(HWND win) {
  RECT rc = {0};

  GetWindowRect(win, &rc);

  LONG WinWidth = rc.right - rc.left;
  LONG WinHeight = rc.bottom - rc.top;

  int ScreenHeight = GetSystemMetrics(SM_CYSCREEN);
  int ScreenWidth = GetSystemMetrics(SM_CXSCREEN);

  SetWindowPos(win, HWND_TOP, (ScreenWidth - WinWidth) / 2,
               (ScreenHeight - WinHeight) / 2, 0, 0, SWP_NOSIZE);
  UpdateWindow(win);
}

#include <stdio.h>
#include <wchar.h>
#include <tchar.h>
#include <windows.h>

int _tmain(int argc, TCHAR *argv[]) {

  PDWORD cChars = NULL;
  HANDLE std = GetStdHandle(STD_OUTPUT_HANDLE);

  if (std == INVALID_HANDLE_VALUE) {
          _tprintf(_T("Cannot retrieve standard output handle\n (%d)"), GetLastError());
  }

  if (argv[1]) {

    WriteConsole(std, argv[1], _tcslen(argv[1]), cChars, NULL);
  }

  CloseHandle(std);

  return 0;
}

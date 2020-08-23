#include <windows.h>
#include <wchar.h>
#include <stdio.h>

int wmain(int argc, WCHAR *argv[])
{

        WCHAR buf[MAX_PATH];

    if(argc != 2) {

        wprintf(L"Usage: %ls <dir>\n", argv[0]);
        return 1;
    }

    BOOL r = SetCurrentDirectoryW(argv[1]);

    if (!r) {

        wprintf(L"SetCurrentDirectoryW() failed (%ld)\n", GetLastError());
        return 1;
    }

    UINT len = GetCurrentDirectoryW(MAX_PATH, buf);

    if (len == 0) {

        wprintf(L"GetCurrentDirectoryW() failed (%ld)\n", GetLastError());
        return 1;
    }

    if (len > MAX_PATH) {

        wprintf(L"Buffer too small; needs %d characters\n", len);
        return 1;
    }

    wprintf(L"Current directory is: %ls\n", buf);

    return 0;
}

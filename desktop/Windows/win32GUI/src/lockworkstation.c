#include <stdio.h>
#include <windows.h>
#include <wchar.h>

int wmain(void)
{
        int r = LockWorkStation();

        if (r == 0) {
                wprintf(L"LockWorkStaton() failed %d\n", GetLastError());
                return 1;
        }

        return 0;
}

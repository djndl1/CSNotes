#include <windows.h>
#include <wchar.h>

#pragma comment(lib, "user32.lib")

int wmain()
{
        int x = GetSystemMetrics(SM_CXSCREEN);
        int y = GetSystemMetrics(SM_CYSCREEN);

        wprintf(L"The screen size is %dx%d\n", x, y);

        return 0;
}

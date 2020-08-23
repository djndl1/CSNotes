#include <windows.h>
#include <wchar.h>

int wmain(void)
{
        WCHAR computerName[MAX_COMPUTERNAME_LENGTH-1];
        DWORD nameSize = sizeof(computerName) / sizeof(computerName[0]);

        SIZE_T r = GetComputerNameW(computerName, &nameSize);

        if (r == 0) {
                wprintf(L"Failed to get computer name %d\n", GetLastError());
                return 1;
        }
        wprintf(L"The computer name is %ls\n", computerName);
        return 0;
}

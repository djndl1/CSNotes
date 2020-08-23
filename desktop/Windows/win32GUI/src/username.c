#include <windows.h>
#include <lmcons.h>
#include <wchar.h>

int wmain()
{
        WCHAR username[UNLEN+1];
        DWORD len = sizeof(username) / sizeof(WCHAR);

        BOOL r = GetUserNameW(username, &len);

        if (!r) {
                wprintf(L"Failed to get username %u\n", GetLastError());
                return 1;
        }
        wprintf(L"the username is %ls\n", username);
        return 0;
}

#include <windows.h>

#include <string>
#include <iostream>
#include <locale>
#include <cwchar>

#include "utf_cvt.h"

using std::string;
using std::wstring;
using std::wstring_convert;
using std::codecvt;

int wmain()
{
    std::string appName{"wineconsole --backend=curses"};

    STARTUPINFO si = { sizeof(si), 0 };
    PROCESS_INFORMATION pi = { 0 };

    std::wstring appNameWide = utf_cvt::utf8_to_utf16(appName);
    wchar_t *appCNameWide = &appNameWide[0];
    BOOL succeess = ::CreateProcessW(nullptr, appCNameWide,
                                     nullptr, nullptr,
                                     FALSE, 0, nullptr,
                                     nullptr, &si, &pi);
    if (!succeess) {
        std::cout << "Created a process" << pi.dwProcessId << '\n';
    } else {
        std::cout << "Failed to create a process" << pi.dwProcessId << "\n";
    }

    return 0;
}

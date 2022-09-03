#include <windows.h>

#include <string>
#include <iostream>
#include <locale>
#include <cwchar>

#include "processes.h"
#include "errors.h"

#include <utility>

using std::string;
using std::wstring;
using std::wstring_convert;
using std::codecvt;

int wmain()
{
    PROCESS_INFORMATION pi = { 0 };
    int error = ::create_process(L"C:\\windows\\system32\\cmd.exe", nullptr, &pi);
    if (!error) {
        std::wcout << "Created a process: " << pi.dwProcessId << '\n';
    } else {
        const wchar_t *error_p = ::get_error_message(error);
        std::wstring error_msg{std::move(error_p)};
        std::wcout << "Failed to create a process: " << error_msg << "\n";
    }

    return 0;
}

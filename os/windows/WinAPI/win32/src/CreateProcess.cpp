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
        std::wcout << L"Created a process: " << pi.dwProcessId << L'\n';
    } else {
        std::wstring error_msg = windows::get_error_message(error);
        std::wcout << L"Failed to create a process: " << error_msg << L"\n";
    }

    ::WaitForSingleObject(pi.hProcess, INFINITE);

    DWORD cmd_exit_code;
    BOOL success = ::GetExitCodeProcess(pi.hProcess, &cmd_exit_code);

    if (success) {
        std::wcout << L"Exit Code: " << cmd_exit_code << L'\n';
    } else {

        std::wstring error_msg = windows::get_error_message(error);
        std::wcout << L"Failed to retrieve the exit code: " << error_msg << L'\n';
    }

    error = ::create_process(L"C:\\windows\\system32\\cmd.exe", nullptr, &pi);
    if (!error) {
        std::wcout << L"Created a process: " << pi.dwProcessId << L'\n';
    } else {
        std::wstring error_msg = windows::get_error_message(error);

        std::wcout << L"Failed to create a process: " << error_msg << L"\n";
    }

    ::TerminateProcess(pi.hProcess, 10);

    success = ::GetExitCodeProcess(pi.hProcess, &cmd_exit_code);

    if (success) {
        std::wcout << L"Exit Code: " << cmd_exit_code << L'\n';
    } else {
        std::wstring error_msg = windows::get_error_message(error);

        std::wcout << L"Failed to retrieve the exit code: " << error_msg << L'\n';
    }

    return 0;
}

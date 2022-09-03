#include <windows.h>
#include <psapi.h>

#include <iostream>
#include <string>

#define EXPANDED_MAX_PATH 32767

HANDLE duplicate_current_process_handle()
{
    HANDLE cur_pseudo = ::GetCurrentProcess();
    HANDLE actual_handle;
    ::DuplicateHandle(
        cur_pseudo,
        cur_pseudo,
        cur_pseudo,
        &actual_handle,
        PROCESS_ALL_ACCESS,
        FALSE,
        DUPLICATE_SAME_ACCESS
    );

    return actual_handle;
}

HANDLE open_current_process_handle()
{
    DWORD cur_pid = ::GetCurrentProcessId();
    return ::OpenProcess(PROCESS_ALL_ACCESS, FALSE, cur_pid);
}

std::wstring show_process_image_path()
{
    std::wstring buffer{};

    size_t buffer_len = EXPANDED_MAX_PATH + 1;
    // cannot use .reserve: direct buffer manipulation is unknown to std::string
    buffer.resize(buffer_len);

    DWORD path_len = ::GetModuleFileNameW(nullptr, buffer.data(), buffer_len);
    buffer.resize(path_len);

    return buffer;
}

std::wstring show_process_image_path_by_module(HANDLE process_handle)
{
    std::wstring buffer{};

    size_t buffer_len = EXPANDED_MAX_PATH + 1;
    // cannot use .reserve: direct buffer manipulation is unknown to std::string
    buffer.resize(buffer_len);

    DWORD path_len = ::GetModuleFileNameExW(process_handle, nullptr, buffer.data(), buffer_len);
    buffer.resize(path_len);

    return buffer;
}

std::wstring show_process_image_namespace_path(HANDLE process_handle)
{
    std::wstring buffer{};

    size_t buffer_len = EXPANDED_MAX_PATH + 1;
    // cannot use .reserve: direct buffer manipulation is unknown to std::string
    buffer.resize(buffer_len);

    DWORD path_len = ::GetProcessImageFileNameW(process_handle, buffer.data(), buffer_len);
    buffer.resize(path_len);

    return buffer;
}

std::wstring query_process_image_name(HANDLE process_handle, bool native)
{
    std::wstring buffer{};

    size_t buffer_len = EXPANDED_MAX_PATH + 1;
    // cannot use .reserve: direct buffer manipulation is unknown to std::string
    buffer.resize(buffer_len);
    DWORD result_size = buffer_len;

    ::QueryFullProcessImageNameW(
        process_handle,
        native ? PROCESS_NAME_NATIVE : 0,
        buffer.data(),
        &result_size);
    buffer.resize(result_size);

    return buffer;
}

int wmain()
{
    HANDLE cur_pseudo = ::GetCurrentProcess();
    DWORD cur_pid = ::GetCurrentProcessId();

    std::wcout << L"Current PID: " << cur_pid << L'\n'
        << L"Current Psuedohandle: " << cur_pseudo << L'\n';

    HANDLE duplicated = duplicate_current_process_handle();
    HANDLE opened = open_current_process_handle();
    std::wcout << L"Duplicated Current Process Handle " << duplicated << L'\n'
        << L"Opened Current Process Handle " << opened << L'\n';

    std::wcout << L"Image Path of the current process: " << L'\n'
               << show_process_image_path() << L'\n';

    std::wcout << L"Show Image path of the handles" << L'\n'
               << L"Pseudo: " << show_process_image_path_by_module(cur_pseudo) << L", "
               << show_process_image_namespace_path(cur_pseudo) << L'\n'
               << L"Duplicated: " << show_process_image_path_by_module(duplicated) << L", "
               << show_process_image_namespace_path(duplicated) << L'\n'
               << L"Opened: " << show_process_image_path_by_module(opened) << L", "
               << show_process_image_namespace_path(opened) << L'\n'
               << L"Query Full Name: " << query_process_image_name(cur_pseudo, false) << L", "
               << query_process_image_name(cur_pseudo, true) << L'\n';

    ::CloseHandle(duplicated);
    ::CloseHandle(opened);

    return 0;
}

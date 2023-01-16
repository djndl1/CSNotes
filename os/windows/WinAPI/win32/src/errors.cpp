#include "errors.h"
#include <cwchar>

#include <windows.h>
#include <stdlib.h>
#include <exception>
#include <utility>

const wchar_t *get_error_message(int error)
{
    LPWSTR message;
    DWORD num = ::FormatMessageW(
        FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
        nullptr,
        error,
        MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
        (LPWSTR)&message,
        0,
        nullptr);

    if (num == 0) {
        return nullptr;
    } else {
        wchar_t *buffer = (wchar_t*) ::malloc(sizeof(wchar_t)*(num+1));
        if (buffer == nullptr) {
            goto success_ret;
        }
        ::wcscpy(buffer, message);

success_ret:
        ::LocalFree(message);
        return buffer;
    }
}

std::wstring windows::get_error_message(int error)
{
    const wchar_t *msg_p = ::get_error_message(error);
    if (msg_p == nullptr) {
        throw windows::message_format_error(error, L"Cannot find message for error " + error);
    }
    return std::wstring{std::move(msg_p)};
}

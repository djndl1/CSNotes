#pragma once

#include <windows.h>

#if defined (__cplusplus)
extern "C" {
#endif

int create_process(const wchar_t*, const wchar_t* [], LPPROCESS_INFORMATION process);

#if defined (__cplusplus)
}
#endif

#include <wchar.h>
#include <windows.h>
#include <stdint.h>
#include <inttypes.h>

int wmain(void) {

   uint64_t freeCall, total, free;

  BOOL r = GetDiskFreeSpaceExW(L"C:\\", (PULARGE_INTEGER)&freeCall,
                              (PULARGE_INTEGER)&total, (PULARGE_INTEGER)&free);

  if (!r) {

    wprintf(L"Failed to get free disk space %ld", GetLastError());
    return 1;
  }

  wprintf(L"Available space to caller: %" PRIu64 L" MB\n", freeCall / (1024 * 1024));
  wprintf(L"Total space: %" PRIu64 L" MB\n", total / (1024 * 1024));
  wprintf(L"Free space on drive: %" PRIu64 L" MB\n", free / (1024 * 1024));

  return 0;
}

#include <windows.h>
#include <wchar.h>

int wmain(void) {

    SYSTEMTIME st = {0};
  
    GetLocalTime(&st);
  
    wprintf(L"The time is: %02d:%02d:%02d\n", 
        st.wHour, st.wMinute, st.wSecond);

    return 0;
}

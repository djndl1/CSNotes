#include <windows.h>
#include <tchar.h>

int WINAPI _tWinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
                     LPTSTR szCmdLine, int nCmdShow)
{
        MessageBox(NULL, szCmdLine, _T("Title"), MB_OK);

        return 0;
}

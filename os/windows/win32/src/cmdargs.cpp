#include <windows.h>
#include <stdio.h>
#include <wchar.h>

int wWinMain(HINSTANCE hInstance, HINSTANCE, LPWSTR lpCmdLine, int nCmdShow)
{
    AllocConsole();
    int count;
    LPWSTR cmdText = GetCommandLine();
    LPWSTR *args = CommandLineToArgvW(cmdText, &count);


    wchar_t text[1024] = { 0 };
    for (int i = 0; i < count; i++) {
        ::wcscat_s(text, 1024, args[i]);
        ::wcscat_s(text, 1024, L" ");
    }
    ::LocalFree(args);

    ::wprintf(L"%ls\n", text);

    return 0;
}

#include <windows.h>

void CenterWindow(HWND win)
{
        RECT rc = {0};

        GetWindowRect(win, &rc);

        LONG WinWidth = rc.right - rc.left;
        LONG WinHeight = rc.bottom - rc.top;

        int ScreenHeight = GetSystemMetrics(SM_CYSCREEN);
        int ScreenWidth = GetSystemMetrics(SM_CXSCREEN);

        SetWindowPos(win, HWND_TOP, (ScreenWidth - WinWidth) / 2, (ScreenHeight - WinHeight) / 2, 0, 0, SWP_NOSIZE);
        UpdateWindow(win);
}

LRESULT CALLBACK WndProc(HWND win, UINT msg, WPARAM wParam, LPARAM lParam)
{
        switch (msg) {
        case WM_CREATE:
                  CenterWindow(win);
                break;
        case WM_DESTROY:
                PostQuitMessage(0);
                break;
        }
        return DefWindowProc(win, msg, wParam, lParam);
}

int wWinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
             LPWSTR lpCmdLine, int nShowCmd)
{
  WNDCLASSEX wc = {.cbSize = sizeof(WNDCLASSEX),
                   .style = WS_OVERLAPPEDWINDOW,
                   .lpszClassName = L"Center",
                   .cbClsExtra = 0,
                   .cbWndExtra = 0,
                   .hInstance = hInstance,
                   .hbrBackground = GetSysColorBrush(COLOR_WINDOW),
                   .lpfnWndProc = WndProc,
                   .hCursor = LoadCursor(NULL, IDC_ARROW),
                   .lpszMenuName = NULL,
                   .hIcon = LoadCursor(hInstance, IDI_APPLICATION),
                   .hIconSm = LoadCursor(hInstance, IDI_APPLICATION)};
  RegisterClassEx(&wc);
  HWND CenterWin =
      CreateWindowEx(0, wc.lpszClassName, L"CenterWindow", wc.style,
                     100, 100, 250, 250, NULL, NULL, hInstance, NULL);
  ShowWindow(CenterWin, SW_SHOWDEFAULT);
  UpdateWindow(CenterWin);

  MSG msg;
  while (GetMessage(&msg, NULL, 0, 0)) {
    TranslateMessage(&msg);
    DispatchMessage(&msg);
        }

        return (int) msg.wParam;
}

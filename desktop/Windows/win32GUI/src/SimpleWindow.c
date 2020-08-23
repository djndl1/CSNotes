#include <windows.h>

void CenterWindow(HWND win) {
  RECT rc = {0};

  GetWindowRect(win, &rc);

  LONG WinWidth = rc.right - rc.left;
  LONG WinHeight = rc.bottom - rc.top;

  int ScreenHeight = GetSystemMetrics(SM_CYSCREEN);
  int ScreenWidth = GetSystemMetrics(SM_CXSCREEN);

  SetWindowPos(win, HWND_TOP, (ScreenWidth - WinWidth) / 2,
               (ScreenHeight - WinHeight) / 2, 0, 0, SWP_NOSIZE);
  UpdateWindow(win);
}

LRESULT CALLBACK WndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
        switch (msg) {
        case WM_CREATE:
                CenterWindow(hwnd);
                break;
        case WM_DESTROY:
                PostQuitMessage(0);
                break;
        }

        return DefWindowProc(hwnd, msg, wParam, lParam);
}

int WINAPI wWinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
                     PWSTR pCmdLine, int nCmdShow)
{
        WNDCLASSEX wc = {
                .cbSize = sizeof(WNDCLASSEX),
                .style = WS_OVERLAPPEDWINDOW | WS_VSCROLL | WS_HSCROLL,
                .lpfnWndProc = WndProc,
                .cbClsExtra = 0,
                .cbWndExtra = 0,
                .hInstance = hInstance,
                .hCursor = LoadCursor(NULL, IDC_ARROW),
                .hbrBackground = GetSysColorBrush(COLOR_WINDOW),
                .lpszClassName = L"myWindow",
                .lpszMenuName = NULL,
                .hIcon = LoadCursor(hInstance, IDI_APPLICATION),
                .hIconSm = LoadCursor(hInstance, IDI_APPLICATION)
        };
        RegisterClassEx(&wc);
        HWND mywindow = CreateWindowEx(0, wc.lpszClassName, L"Window",
                                       wc.style, CW_USEDEFAULT, CW_USEDEFAULT,
                                       CW_USEDEFAULT, CW_USEDEFAULT,
                                       NULL, NULL, hInstance, NULL);
        ShowWindow(mywindow, SW_SHOWDEFAULT);
        UpdateWindow(mywindow);

        MSG msg;
        while (GetMessage(&msg, NULL, 0, 0)) {
                DispatchMessage(&msg);
        }

        return (int) msg.wParam;
}

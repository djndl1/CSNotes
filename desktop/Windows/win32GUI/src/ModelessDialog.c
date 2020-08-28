#include <windows.h>

LRESULT CALLBACK WinProc(HWND win, UINT msg, WPARAM wParam, LPARAM lParam);

LRESULT CALLBACK DialogProc(HWND win, UINT msg, WPARAM wParam, LPARAM lParam);

HINSTANCE ghInstance;

void RegisterDialogClass(HWND win)
{
    WNDCLASSEX wc = {0};
    wc.cbSize = sizeof(WNDCLASSEX);
    wc.lpfnWndProc = (WNDPROC) DialogProc;
    wc.hInstance = ghInstance;
    wc.hbrBackground = GetSysColorBrush(COLOR_WINDOW);
    wc.lpszClassName = L"DialogClass";

    RegisterClassEx(&wc);
}

void CreateDialogBox(HWND win)
{
    CreateWindowEx(WS_EX_DLGMODALFRAME | WS_EX_TOPMOST, L"DialogClass", L"Dialog Box",
                   WS_VISIBLE | WS_SYSMENU | WS_CAPTION, 100, 100, 200 ,150,
                   NULL, NULL, ghInstance, NULL);
}

LRESULT CALLBACK WinProc(HWND win, UINT msg, WPARAM wParam, LPARAM lParam)
{
    switch(msg) {
        case WM_CREATE:
        RegisterDialogClass(win);
        CreateWindow(L"button", L"Show Dialog", WS_CHILD | WS_VISIBLE,
                     20, 50, 95, 25, win, (HMENU) 1, NULL, NULL);
        break;

    case WM_COMMAND:
        CreateDialogBox(win);
        break;

    case WM_DESTROY:
        PostQuitMessage(0);
        return 0;
    }

    return DefWindowProcW(win, msg, wParam, lParam);
}

LRESULT CALLBACK DialogProc(HWND win, UINT msg, WPARAM wParam, LPARAM lParam)
{
    switch (msg) {
    case WM_CREATE:
        CreateWindow(L"button", L"ok", WS_VISIBLE | WS_CHILD,
                     50, 50, 80 ,25, win, (HMENU) 1, NULL, NULL);
        break;

    case WM_COMMAND:
        DestroyWindow(win);
        break;

    case WM_CLOSE:
        DestroyWindow(win);
        break;
        
    }

    return DefWindowProc(win, msg,  wParam,  lParam);
}


int WINAPI wWinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
    PWSTR pCmdLine, int nCmdShow) {

  MSG  msg;
  HWND hwnd;

  WNDCLASSW wc = {0};

  wc.lpszClassName = L"Window";
  wc.hInstance     = hInstance;
  wc.hbrBackground = GetSysColorBrush(COLOR_WINDOW);
  wc.lpfnWndProc   = WinProc;

  RegisterClassW(&wc);
  hwnd = CreateWindowW(wc.lpszClassName, L"Window",
                WS_OVERLAPPEDWINDOW | WS_VISIBLE,
                100, 100, 250, 150, NULL, NULL, hInstance, NULL);

  ghInstance = hInstance;

  while( GetMessage(&msg, NULL, 0, 0)) {
    DispatchMessage(&msg);
  }

  return (int) msg.wParam;
}

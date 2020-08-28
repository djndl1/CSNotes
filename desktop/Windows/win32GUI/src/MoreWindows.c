#include <windows.h>
#include <wchar.h>
#include <strsafe.h>

#define ID_HOTKEY 1

HWND hwndSta1;
HWND hwndSta2;

LRESULT CALLBACK PanelProc(HWND hwnd, UINT msg, 
    WPARAM wParam, LPARAM lParam) {
    
    switch(msg) {
  
        case WM_LBUTTONUP:
            MessageBeep(MB_OK);
            break;    
    }

    return DefWindowProcW(hwnd, msg, wParam, lParam);
}

void RegisterRedPanelClass(void) {

    HBRUSH hbrush = CreateSolidBrush(RGB(255, 0, 0));

    WNDCLASSW rwc = {0};

    
    rwc.lpszClassName = L"RedPanelClass";
    rwc.hbrBackground = hbrush;
    rwc.lpfnWndProc   = PanelProc;
    rwc.hCursor       = LoadCursor(0, IDC_ARROW);
    RegisterClassW(&rwc); 
}

void RegisterBluePanelClass(void) {

    HBRUSH hbrush = CreateSolidBrush(RGB(0, 0, 255));

    WNDCLASSW rwc = {0};
    
    rwc.lpszClassName = L"BluePanelClass";
    rwc.hbrBackground = hbrush;
    rwc.lpfnWndProc   = PanelProc;
    rwc.hCursor       = LoadCursor(0, IDC_ARROW);

    RegisterClassW(&rwc);
}

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

void CreateLabels(HWND hwnd) {

    CreateWindowW(L"static", L"x: ",
        WS_CHILD | WS_VISIBLE,
        10, 10, 25, 25, 
        hwnd, (HMENU) 1, NULL, NULL);

    hwndSta1 = CreateWindowW(L"static", L"150",
        WS_CHILD | WS_VISIBLE,
        40, 10, 55, 25, 
       hwnd, (HMENU) 2, NULL, NULL);

    CreateWindowW(L"static", L"y: ",
        WS_CHILD | WS_VISIBLE,
        10, 30, 25, 25, 
        hwnd, (HMENU) 3, NULL, NULL);

    hwndSta2 = CreateWindowW(L"static", L"150",
        WS_CHILD | WS_VISIBLE,
        40, 30, 55, 25, 
        hwnd, (HMENU) 4, NULL, NULL);
}

LRESULT CALLBACK WndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
  RECT rect;
  wchar_t buf[10];

  switch (msg) {
  case WM_CREATE:
          CreateLabels(hwnd);
          RegisterHotKey(hwnd, ID_HOTKEY, MOD_CONTROL, 0x43);
    RegisterRedPanelClass();

    CreateWindowW(L"RedPanelClass", NULL, WS_CHILD | WS_VISIBLE, 20, 20, 80, 80,
                  hwnd, (HMENU)1, NULL, NULL);

    RegisterBluePanelClass();

    CreateWindowW(L"BluePanelClass", NULL, WS_CHILD | WS_VISIBLE, 120, 20, 80,
                  80, hwnd, (HMENU)2, NULL, NULL);
    break;
  case WM_KEYDOWN:
    if (wParam == VK_ESCAPE) {
      int ret =
          MessageBox(hwnd, L"Are you sure to quit?", L"Message", MB_OKCANCEL);
      if (ret == IDOK)
        SendMessage(hwnd, WM_CLOSE, 0, 0);
    }
    break;
  case WM_HOTKEY:
    if (wParam == ID_HOTKEY)
      CenterWindow(hwnd);
    break;
  case WM_MOVE:

    GetWindowRect(hwnd, &rect);

    swprintf_s(buf, 10, L"%ld", rect.left);
    SetWindowTextW(hwndSta1, buf);

    swprintf_s(buf, 10, L"%ld", rect.top);
    SetWindowTextW(hwndSta2, buf);
    break;
  case WM_DESTROY:
    UnregisterHotKey(hwnd, ID_HOTKEY);
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
                .lpszClassName = L"hotkey",
                .lpszMenuName = NULL,
                .hIcon = LoadCursor(hInstance, IDI_APPLICATION),
                .hIconSm = LoadCursor(hInstance, IDI_APPLICATION)
        };
        RegisterClassEx(&wc);
        HWND mywindow = CreateWindowEx(0, wc.lpszClassName, L"hotkey",
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

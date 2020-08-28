#include <windows.h>

LRESULT CALLBACK WndProc(HWND, UINT, WPARAM, LPARAM);
LRESULT CALLBACK PanelProc(HWND, UINT, WPARAM, LPARAM);

void RegisterPanel(void);
COLORREF ShowColorDialog(HWND);

COLORREF gColor = RGB(255, 255, 255);

int WINAPI wWinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
    PWSTR pCmdLine, int nCmdShow) {

  MSG  msg ;
  WNDCLASSW wc = {0};
  wc.lpszClassName = L"Color dialog box";
  wc.hInstance     = hInstance;
  wc.hbrBackground = GetSysColorBrush(COLOR_3DFACE);
  wc.lpfnWndProc   = WndProc;

  RegisterClassW(&wc);
  CreateWindowW( wc.lpszClassName, L"Color dialog box",
                WS_OVERLAPPEDWINDOW | WS_VISIBLE,
                150, 150, 250, 200, 0, 0, hInstance, 0);

  while( GetMessage(&msg, NULL, 0, 0)) {
    DispatchMessage(&msg);
  }

  return (int) msg.wParam;
}

LRESULT CALLBACK WndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam) {

  static HWND hwndPanel;

  switch(msg) {

    case WM_CREATE:
    {
        CreateWindowW(L"button", L"Color",
           WS_VISIBLE | WS_CHILD ,
           20, 30, 80, 25,
           hwnd, (HMENU) 1, NULL, NULL);

        RegisterPanel();
        hwndPanel = CreateWindowW(L"Panel", NULL,
                    WS_CHILD | WS_VISIBLE,
                    130, 30, 80, 80, hwnd, (HMENU) 2, NULL, NULL);
        break;
    }

    case WM_COMMAND:
    {
        gColor = ShowColorDialog(hwnd);
        InvalidateRect(hwndPanel, NULL, TRUE);
        break;
    }

    case WM_DESTROY:
    {
        PostQuitMessage(0);
        break;
    }
  }

  return DefWindowProcW(hwnd, msg, wParam, lParam);
}

LRESULT CALLBACK PanelProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam) {

  HDC hdc;
  PAINTSTRUCT ps;
  RECT rect;

  switch(msg) {

    case WM_PAINT:
    {
        GetClientRect(hwnd, &rect);
        hdc = BeginPaint(hwnd, &ps);
        SetBkColor(hdc, gColor);
        ExtTextOut(hdc, 0, 0, ETO_OPAQUE, &rect, L"", 0, NULL);
        EndPaint(hwnd, &ps);
        break;
    }
  }

  return DefWindowProc(hwnd, msg, wParam, lParam);
}


COLORREF ShowColorDialog(HWND hwnd) {

  CHOOSECOLOR cc;
  static COLORREF crCustClr[16];

  ZeroMemory(&cc, sizeof(cc));
  cc.lStructSize = sizeof(cc);
  cc.hwndOwner = hwnd;
  cc.lpCustColors = (LPDWORD) crCustClr;
  cc.rgbResult = RGB(0, 255, 0);
  cc.Flags = CC_FULLOPEN | CC_RGBINIT;
  ChooseColor(&cc);

  return cc.rgbResult;
}

void RegisterPanel(void) {

  WNDCLASSW rwc = {0};
  rwc.lpszClassName = L"Panel";
  rwc.hbrBackground = (HBRUSH) GetStockObject(WHITE_BRUSH);
  rwc.lpfnWndProc   = PanelProc;
  RegisterClassW(&rwc);
}

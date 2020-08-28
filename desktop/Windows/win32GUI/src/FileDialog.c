#include <windows.h>

LRESULT CALLBACK WndProc(HWND, UINT, WPARAM, LPARAM);
void CreateMenubar(HWND);
void OpenDialog(HWND);
void LoadFile(LPWSTR);

#define IDM_FILE_NEW 1
HWND ghwndEdit;


int WINAPI wWinMain( HINSTANCE hInstance, HINSTANCE hPrevInstance,
    LPWSTR lpCmdLine, int nCmdShow) {

  MSG  msg ;
  WNDCLASS wc = {0};
  wc.lpszClassName = TEXT( "Opendialog" );
  wc.hInstance     = hInstance ;
  wc.hbrBackground = GetSysColorBrush(COLOR_3DFACE);
  wc.lpfnWndProc   = WndProc ;
  wc.hCursor       = LoadCursor(0, IDC_ARROW);


  RegisterClass(&wc);
  CreateWindow( wc.lpszClassName, TEXT("Opendialog"),
                WS_OVERLAPPEDWINDOW | WS_VISIBLE,
                150, 150, 265, 200, 0, 0, hInstance, 0);

  while( GetMessage(&msg, NULL, 0, 0)) {
    DispatchMessage(&msg);
  }

  return (int) msg.wParam;
}

LRESULT CALLBACK WndProc( HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam) {

  switch(msg) {

      case WM_CREATE:
          ghwndEdit = CreateWindowEx(WS_EX_RIGHTSCROLLBAR, TEXT("edit"), NULL,
                    WS_VISIBLE | WS_CHILD | WS_HSCROLL | WS_VSCROLL | ES_MULTILINE,
                    0, 0, 260, 180,
                    hwnd, (HMENU) 1, NULL, NULL);


          CreateMenubar(hwnd);
          break;

      case WM_SIZE:
          SetWindowPos(ghwndEdit, NULL, 0, 0, LOWORD(lParam), HIWORD(lParam),
             SWP_NOMOVE | SWP_NOZORDER);
          break;

      case WM_COMMAND:
          if (wParam==IDM_FILE_NEW) {
              OpenDialog(hwnd);
          }
          break;

      case WM_DESTROY:
          PostQuitMessage(0);
          break;
  }

  return DefWindowProc(hwnd, msg, wParam, lParam);
}

void CreateMenubar(HWND hwnd) {

  HMENU hMenubar;
  HMENU hMenu;

  hMenubar = CreateMenu();
  hMenu = CreateMenu();
  AppendMenu(hMenubar, MF_POPUP, (UINT_PTR)hMenu, TEXT("&File"));
  AppendMenu(hMenu, MF_STRING, IDM_FILE_NEW, TEXT("&Open"));
  SetMenu(hwnd, hMenubar);
}

void OpenDialog(HWND hwnd) {

  OPENFILENAME ofn;
  TCHAR szFile[MAX_PATH];

  ZeroMemory(&ofn, sizeof(ofn));
  ofn.lStructSize = sizeof(ofn);
  ofn.lpstrFile = szFile;
  ofn.lpstrFile[0] = '\0';
  ofn.hwndOwner = hwnd;
  ofn.nMaxFile = sizeof(szFile);
  ofn.lpstrFilter = TEXT("All files(*.*)\0*.*\0");
  ofn.nFilterIndex = 1;
  ofn.lpstrInitialDir = NULL;
  ofn.lpstrFileTitle = NULL;
  ofn.Flags = OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST;

  if(GetOpenFileName(&ofn))
      LoadFile(ofn.lpstrFile);
}

void LoadFile(LPWSTR file) {

  HANDLE hFile;
  DWORD dwSize;
  DWORD dw;

  LPBYTE lpBuffer = NULL;

  hFile = CreateFile(file, GENERIC_READ, 0, NULL, OPEN_EXISTING, 0, NULL);
  dwSize = GetFileSize(hFile, NULL);
  lpBuffer = (LPBYTE) HeapAlloc(GetProcessHeap(),
      HEAP_GENERATE_EXCEPTIONS, dwSize + 1);
  ReadFile(hFile, (LPWSTR)lpBuffer, dwSize, &dw, NULL);
  CloseHandle(hFile);
  lpBuffer[dwSize] = 0;
  SetWindowTextA(ghwndEdit, (LPSTR) lpBuffer);
  HeapFree(GetProcessHeap(), 0, lpBuffer);
}

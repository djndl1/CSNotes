#include <windows.h>

#define IDM_FILE_NEW 1
#define IDM_FILE_OPEN 2
#define IDM_FILE_QUIT 3

void AddMenu(HWND win)
{

    HMENU fileMenu  = CreateMenu();
    AppendMenu(fileMenu, MF_STRING, IDM_FILE_NEW, L"&New");
    AppendMenu(fileMenu, MF_STRING, IDM_FILE_OPEN, L"&Open");
    AppendMenu(fileMenu, MF_STRING, IDM_FILE_QUIT, L"&Quit");
    
    HMENU mainMenuBar = CreateMenu();
    AppendMenu(mainMenuBar, MF_POPUP, (UINT_PTR) fileMenu, L"&File");

    SetMenu(win, mainMenuBar);
}

HMENU makeContextMenu(HWND win)
{
    HMENU contextMenu = CreatePopupMenu();

    AppendMenu(contextMenu, MF_STRING, IDM_FILE_NEW, L"&New");
    AppendMenu(contextMenu, MF_STRING, IDM_FILE_OPEN, L"&Open");
    AppendMenu(contextMenu, MF_STRING, IDM_FILE_QUIT, L"&Quit");

    return contextMenu;
}

LRESULT CALLBACK WinProc(HWND win, UINT msgId, WPARAM wParam, LPARAM lParam)
{
    POINT clickedPoint;
    
    switch(msgId){
    case WM_CREATE:
        AddMenu(win);
        break;
    case WM_COMMAND:
        switch(LOWORD(wParam)){
        case IDM_FILE_NEW:
        case IDM_FILE_OPEN:
            MessageBeep(MB_ICONWARNING);
            break;
        case IDM_FILE_QUIT:
            SendMessage(win, WM_CLOSE, 0, 0);
            break;
        }
        break;
        case WM_RBUTTONUP:
            clickedPoint.x = LOWORD(lParam);
            clickedPoint.y = HIWORD(lParam);
            ClientToScreen(win, &clickedPoint);

            HMENU contextMenu = makeContextMenu(win);
            TrackPopupMenu(contextMenu, TPM_CENTERALIGN, clickedPoint.x, clickedPoint.y,
                           0, win, NULL);
            DestroyMenu(contextMenu);
            
            break;
    case WM_DESTROY:
        PostQuitMessage(0);
        break;
    }
    return DefWindowProc(win, msgId, wParam, lParam);
}

int WINAPI wWinMain(HINSTANCE thisInst, HINSTANCE prevInst, PWSTR cmdLine, int nArgs)
{
    WNDCLASSEX wc = {0};
    wc.cbSize = sizeof(wc);
    wc.lpszClassName = L"Menu Window";
    wc.hInstance = thisInst;
    wc.hbrBackground = GetSysColorBrush(COLOR_WINDOW);
    wc.lpfnWndProc = WinProc;
    wc.hCursor = LoadCursor(0, IDC_ARROW);

    RegisterClassEx(&wc);
    HWND mainWin = CreateWindowEx(0, wc.lpszClassName, L"MenuWin", WS_OVERLAPPEDWINDOW,
                   200, 200, 250, 300, NULL, NULL, thisInst, NULL);
    ShowWindow(mainWin, SW_NORMAL);

    MSG msg;
    while (GetMessage(&msg, NULL, 0, 0)) {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }

    return (int) msg.wParam;
}

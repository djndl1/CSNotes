The Windows API can be divided into

1. Base services: file systems; devices; processes; threads; registry; error handling

2. Security: authentication, authorization; cryptography; 

3. Graphics

4. User Interface

5. Multimedia

6. Windows Shell

7. Networking

# String Functions

Besides standard C string functions, Windows has some kernel and shell string functions.

`lstrlen`; `lstrcat`; `CharLower`; `CharUpper`; `lstrcmp`; `lstrcmpi`; `wsprintfW`; `GetStringType`.

`StrTrim`; `StrToIntEx`; `StrStrW`

To increase application security, StrSafe functions were released. 

`StringCchLength`; `StringCbLength`; `StringCchGetsW`; `StringCchCopyW`; `StringCchCatW`; `StringCchPrintfW`

# Data and Time

struct `SYSTEMTIME` is broken-down date, filled with `GetSystemTime` (UTC time) or `GetLocalTime`:

```c
WORD wYear;
WORD wMonth;
WORD wDayOfWeek;
WORD wDay;p
WORD wHour;
WORD wMinute;
WORD wSecond;
WORD wMilliseconds;
```

and `FILETIME`:

```c
DWORD dwLowDateTime;
DWORD dwHighDateTime;
```

Date arithemtic is done by converting `SYSTEMTIME` to `FILETIME` and back.

`GetDateFormatEx` formats the date in a spcified locale.

`isLeapYear`. `GetTickCount` (uptime)

Windows operating system has several epochs. Microsoft Excel, MS SQL Server or FAT32 filesystem have different time epochs. The Windows API epoch is January 1, 1601, 00:00:00 UTC. The reason for choosing this date was the 400th anniversary of accepting of the Gregorian calendar. The anniversary fall at the time when Windows NT was designed. The FILETIME structure contains a 64-bit value representing the number of 100-nanosecond intervals since January 1, 1601 (UTC). 

`CompareFIleTIme`; `GetTimeZoneInformation`

# Window

Everything GUI is a window. Only one window at a time can receive input from the user.

## Desktop Window

the desktop IS a window. The background is the wallpaper.

`GetDesktopWindow`; 

## Application Windows

```
+------|-----------------------------------|-----------|-|----+
|icon  |      Title Bar                    |    ||     | |  |-|
|      |                                   +-----|     +-+ +--|
+------|-----|----------|----------|--------------------------+
|            |          |          |                          |
|            |          |          |                          |  Menu Bar
+------------|----------|----------|--------------------------+
|                                                        +----+
|                                                        |    |
|                                                        |    |
|                                                        +----+
|                                                        |    |
|                                                        +----+
|                                                        |    |  Vertical
|                  Client Area                           |    | Scroll bar
|                                                        |    |
|                                                        |    |
|                                                        |    |
|                                                        |    |
|                                                        |    |
|                                                        |    Sizing border
|                                                        |    |
+--------------------------------------------------------|----+
|        Horizontal scroll bar                                |
|                                                             |
+-------------------------------------------------------------+
|                                                             |
+-------------------------------------------------------------+
```

The app must provide a _window procedure_ to process input to the window
and display output in the client area. The client area can be divided into different functional areas using child windows.

An app can create several types of windows in addtion to its main window, including _controls_ and _dialog boxes_.

_control_: a window used to obtain a specific piece of information from the user.

_dialog box_: a window that contains one or more controls.

An application must provide the following _attributes_ to create a window:

- Class Name: every window belongs to a class, which defines the appearance and behavior. The chief component is _window procedure_, a function that receives and processes all input and requests sent to the window.

- Window Name: a text string that identifies a window for the user,  typically in the title bar. `SetWindowText`, `GetWindowText`

- Window Style (and extended style) `WS_` (mostly used `WS_OVERLAPPEDWINDOW`): named constraint that defines an aspect of the window's appearance and behavior not specified by the class. `SetWindowLong`.

- Position and Size: in pixels; can be set to default `GetSystemMetrics`; `AdjustWindowRect(Ex)` calculates the required size of a window based on the desired size of the client area.

- Parent or Owner Window Handle: The parent window provides the coordinate system for positioning a child window. A window that has no parent is a top-level window. `EnumWindows` passes the handle to each top-level window.

- Menu Handle or Child-Window Identifier

- App instance handle

- Creation Data: a pointer is passed to the window procedure of the window being created, which can be used to initialize app-defined variables.

- Window Handle: `HWND`. `FindWindow(Ex)`; `IsWindow`;

## Window Creation

`CreateWindow(Ex)`

`WinMain` registers the window class (`RegisterClass`), creates the main window (`CreateWindowEx`). `ShowWindow` then display the main window.

messages are sent: `WM_NCCREATE` (non-client area), `WM_CREATE` (client area), both of which contain a `CREATESTRUCT` pointer which contains all the info specified by `CreateWindowEx`. A `WM_PARENTNOTIFY` is also sent to the parent when creating a child window.

## Window Destroy

`DestroyWindow`, `WM_CLOSE` is sent when close box is clicked, the application then calls `DestroyWindow` if confirmed, which then sends `WM_DESTROY`. A main window its processing of `WM_DESTROY` by calling the `PostQuitMessage` function to quit the application.

_messsage_ = event, stored in a message queue. `GetMessage()`, `DispatchMessage`, `TranslateMessasge`

`RegisterClassW` Register a window before creating one. `CreateWindowW`;
Every window has an associated window procedure. It is a function that receives messages.

## Dialog

A dialog is only a special kind of a window, created as a normal window with some specific flags.

```c
CreateWindowExW(WS_EX_DLGMODALFRAME | WS_EX_TOPMOST,  L"DialogClass", L"Dialog Box", WS_VISIBLE | WS_SYSMENU | WS_CAPTION , 100, 100, 200, 150, 
    NULL, NULL, ghInstance,  NULL);
```

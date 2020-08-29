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

`WinMain` registers the window class (`RegisterClass`), creates the main window (`CreateWindowEx`). `ShowWindow` then display the main window. `RegisterClassW` Register a window before creating one. `CreateWindowW`; Every window has an associated window procedure. It is a function that receives messages.

_messsage_ = event, stored in a message queue. `GetMessage()`, `DispatchMessage`, `TranslateMessasge`

messages are sent: `WM_NCCREATE` (non-client area), `WM_CREATE` (client area), both of which contain a `CREATESTRUCT` pointer which contains all the info specified by `CreateWindowEx`. A `WM_PARENTNOTIFY` is also sent to the parent when creating a childnya window.

## Window Destroy

`DestroyWindow`, `WM_CLOSE` is sent when close box is clicked, the application then calls `DestroyWindow` if confirmed, which then sends `WM_DESTROY`. A main window its processing of `WM_DESTROY` by calling the `PostQuitMessage` function to quit the application.

Destroying a window also destorys the window's descendant windows, wit h`WM_DESTROY` first sent to the window, then to its child windows and descendant widnows. 

## Windows Types

- __Overlapped Window_ `WS_OVERLAPPED`/`WS_OVERLAPPEDWINDOW`:has a title bar, border and client area, meant to serve as an application's main window.

- _pop-up window_ `WS_POP`, `WS_POPUPWINDOW`, `WS_CAPTION` for a title bar: used for dialog boxes, message boxes and other window that appear outside an application's main window. 

- _child window_ `WS_CHILD`: confined to the client area of its parent window to divide the client area into functional areas. Must have a parent window specified when calling `CreateWindowEx`. A child window has a client area but no other features unless explicitly requested (a menu is not allowed). The position is relative to the upple left corner of its parent window's client area. The part that extends beyond the border of the client area is clipped. Actions (`Destroyed`, `Hidden`, `Moved`, `Shown`) that affect the parent  window  can also affect the child window. Sibling window can draw in each other's client area. An application can change the parent window of an existing child window by calling the `SetParent` function. The `GetParent` retrieves a handle to the parent window. `IsChild`; `EnumChildDiwndows`A child window can have a unique interger identifier for the messsage to be directed (set when setting the `hMenu` parameter of `CreateWindowEx`).

- _layered window_ `ES_EX_LAYERED`: the system automatically composes and reqpaints layered windows and the windows of underlying applications.

- _message-only windows_ `HWND_MESSAGE` in `hwndParent`: not visible, has no z-order, cannot be enumerated and does not receive broadcast messages. The window simply dipatches messages.

## Window Relationship

The forground thread has a normal base priority of 9; a background thread has a normal base priority of 7. `GetForegroundWindow`; `SetForegroundWindow`

An overlapped or pip-up window can be owned by another overlapped or pop-up window. `WS_OVERLAPPED`/`WS_POPUP` together with `hwndParent` specifying the owner window. `GetWIndow` with `GW_OWNER` to retrieve a handle to a window's owner.

z-order: `BringWindowToTop`; `SetWindowPos`; `DeferWindowPos`; `GetTopWindow`; `GetNextWindow`

## Window Show State

A window may be active/inactive, hidden/visible, minized/maximized, or restored.

- _active window_: the top-level window of the app with which the user is currently working. Only one top-level window in the system is active at a time. 
`SetActiveWindow`; `GetActiveWindow`; `SetWindowPos`; `DeferWindowPos`; `SetWIndowPlacement`. `WM_ACTIVATEAPP`, `WM_ACTIVATE`

- _disabled window_: receives no keyboard or mouse input form the user but can receive messages from other windows, from other applications or from the system. `WS_DISABLED`, `WS_ENABLE`; `EnableWindow`; `IsWIndowEnabled`; When a child window is disabled, the system passes the child's mouse input message to the parent window.

- _visibility_ `WS_VISIBLE`: the system displays a visible window on the screen, hides a hidden window (default) by not drawing it.

- _minimized_ (`WS_MINIMIZE`), _maximized_ (`WS_MAXIMIZE`), _restored_: `ShowWindow` can minimize, maximize or restore a window.  

## Window Size and Position

`CW_USEDEFAULT`for default size and position. Relative to the position of the top-level window created recently (including desktop window); tracking sizes are the sizes produced by dragging the window's sizing borders. `SC_*` system commands (`WM_SYSCOMMAND`) can be used to change the size of positon of the window.

An application can set the window's size or position by calling `SetWindowPlacement`, `MoveWindow`, `SetWindowPos`, `DeferWindowPos`; An application can retrieve the coordinates of a window's bouding rectangle by `GetWindowRect`. `GetClientRect`; `ScreenToClient`; `MapWindowPoints`

## Window Animation

`AnimateWindow`: `AW_SLIDE`; `AW_BLEND`; `AW_CENTER` produces special effects when shwing or hding windows.

## Window Layout and Mirroring

The window layout defines how text and Windows _Graphics Device Interface_ (GDI) objects are laid out in a window or _device context_ (DC).  `WS_EX_LAYOUTRTL` 

## Using Windows

### Creating a main window

A main window belongs to an application defined window class.

`CreateWindowEx`, typically with `WSOVERLAPPEDWINDOW` style to get a title bar, a window menu, a sziing border and minimize and maximize buttons, optional `WS_VSCROLL`, `WS_HSCROLL`.

### Creating, Enumerating and Sizing Child Windows

register the child window class , `CreateWindowEx` the window with `WS_CHILD` and specify a parent window.

## Dialog

A dialog is only a special kind of a window, created as a normal window with some specific flags.

```c
CreateWindowExW(WS_EX_DLGMODALFRAME | WS_EX_TOPMOST,  L"DialogClass", L"Dialog Box", WS_VISIBLE | WS_SYSMENU | WS_CAPTION , 100, 100, 200, 150, 
    NULL, NULL, ghInstance,  NULL);
```

# Window Class

A _window class_ is a set of attributes that the system uses as a template to create a window. Every window is a member of a window class. All window classes are process specific. Each window class has an associated _window procedure_ shared by all windows of the same class.

System classes are registered by the system the first time one of its threads calls a user or GDI function. `Button`, `ComboBox`, `Edit`, `ListBox`, `MDIClient`, `ScrollBar`, `Static`.
An application global class is a window class registered by an executable or DLL that is available to all other modules in the process. 
An application local class is any window class that an executable or .dll registers for its exclusive use. An application can `UnregisterClass` to remove a local class and free the storage associated with it.

The system maintains a list of structures for each of the three types of window classes. Search order: application local -> application global -> system classes

## Registering a Window Class

Fill in a `WNDCLASSEX` and `RegisterClassEx`, `CS_GLOBALCLASS` for application global class. The system determines class ownership from the hInstance member of the WNDCLASSEX structure passed to the RegisterClassEx function when the class is registered. The process must destroy all windows using the class before the .dll is unloaded and call the UnregisterClass function. The system requires only that  an application supply a class name, the window procedure and an instance handle.

Class cursor: the shape of the cursor when it is in the client area of a window in the class. `LoadCursor`.

Class background brush: create a brush by using GDI and assign the returned brush handle. Use `GetSysColorBrush` to retrieve a handle to a brush to corresponds to a standard system color.

class menu: defines the default menu to be used by the windows inthe class if no explicit menu is given when the windows are created.

_device context_: a special set of values that applications use for drawing in theclinetarea of their windows.

Use the _extra class memory_ to store any info pertaining to the class.

# Window Procedure

`DefWindowProc`: default window procedure that defines certain fundamental dbehavior shared by all windows.
 
 TODO subclassing
 
 # Message and Messge Queues
 
 Event-driven apps do not make explicit function calls to obtain input. They wait for the system to pass input to them. The system passes all input for an application to the various windows in the application. Each window has a function, called a window procedure, that the system calls whenever it has input for the window. The window procedure processes the input and returns control to the system. 
 
 If a top-level window stops responding to messages for more than several seconds, the system considers the window to be not responding.
 
 

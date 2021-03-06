# Basics

1. All windows are contained within the root window. The display screen of the X window server is the _root window_. A subwindow must be contained within its parents or be truncated, that is, if parts of the window exceed the expanse of the root window.

2. a parent window alone has a title bar.

3. Menus, buttons, and dialogue boxes are all treated as windows.

4. All length measurements are in screen pixels.

5. Each window contains and carries its own coordinate system.

All X window programming consists of 

1. Creation of a window;

2. Making the window visible;

3. Drawing into that window;

4. Handling input on the window.

X Window is a client and server system, of which the protocol is a series of messages between the two.

An Xlib application program goes through the following steps:

## open connection to the server

- _Screen_: infromtion of the nature of the screen. A phsyical monitor and hardware that can be color, grayscale or monochrome.

- _Visual_: how colors are represented

- _Display_: formation of X protocol packet: byte order, host:display string, default screen number. A set of screens for a single user with one keyboard and one pointer is called a display.


A client program first requests a connection `Display` to be made to a server.

```c
Display *XOpenDisplay(char *display_name);
```

## create a top-level window

A parent window can contain sub-windows and the sub-windows can contain sub-window and so on. All windows are sub-windows of a maste rwindow (root window), controlled by the Window Manager on the server.

```c
Window XCreateWindow(Display *display, Window parent, int x, int y, unsigned int width, unsigned int height, unsigned int border_width, int depth, unsigned int class, Visual *visual, unsigned long valuemask, XSetWindowAttributes *attributes);

Window XCreateSimpleWindow(Display *display, Window parent, int x, int y, unsigned int width, unsigned int height, unsigned int border_width, unsigned long border, unsigned long background);
```

3. give the WM hints

4. establish window resources

5. create all the other windows needed

6. map the windows

7. enter the event loop

8. clean up before exiting


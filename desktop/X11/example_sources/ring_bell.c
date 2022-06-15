#include <X11/Xlib.h>
#include <X11/Xutil.h>

int main(int argc, char *argv[])
{
    Display *my_display = XOpenDisplay("");

    XBell(my_display, 0);

    XCloseDisplay(my_display);

    return 0;
}

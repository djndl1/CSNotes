#include <X11/Xlib.h>
#include <X11/Xutil.h>

int main(int argc, char *argv[])
{
    // open connection
    Display *my_display = XOpenDisplay("");

    // Create top window
    int screen_num = DefaultScreen(my_display);
    unsigned long value_mask = CWBackPixel;
    XSetWindowAttributes window_attr = {
        .background_pixel = WhitePixel(my_display, screen_num)
    };
    Window my_window = XCreateWindow(my_display,
                                     RootWindow(my_display, screen_num),
                                     200, 200, 350, 250, 2,
                                     DefaultDepth(my_display, screen_num),
                                     InputOutput,
                                     DefaultVisual(my_display, screen_num),
                                     value_mask,
                                     &window_attr);

    XMapWindow(my_display, my_window);

    // enter the event loop
    XEvent event;
    XNextEvent(my_display, &event); // necessary for the window to show up

    // clean up before exiting


    return 0;
}

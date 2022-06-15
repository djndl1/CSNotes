#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <stdint.h>
#include <stdbool.h>

static char *WINDOW_NAME = "BASIC";
static char *ICON_NAME = "Ba";


int main(int argc, char *argv[])
{
    // Open a display
    Display *my_display = XOpenDisplay("");

    // create a top-level window
    int screen_num = DefaultScreen(my_display);
    XSetWindowAttributes my_attr = {
        .background_pixel = WhitePixel(my_display, screen_num),
        .border_pixel = BlackPixel(my_display, screen_num),
        .event_mask = ButtonPressMask,
    };
    unsigned long value_mask = CWBackPixel | CWBorderPixel | CWEventMask;

    Window window = XCreateWindow(my_display,
                                  RootWindow(my_display, screen_num),
                                  200, 200, 350, 250, 2,
                                  DefaultDepth(my_display, screen_num),
                                  InputOutput,
                                  DefaultVisual(my_display, screen_num), value_mask, &my_attr);

    XSizeHints wmsize = {
        .flags = USPosition | USSize
    };
    XSetWMNormalHints(my_display, window, &wmsize);

    XWMHints hints = {
        .initial_state = NormalState,
        .flags = StateHint,
    };
    XSetWMHints(my_display, window, &hints);

    XTextProperty window_name;
    XStringListToTextProperty(&WINDOW_NAME, 1, &window_name);
    XSetWMName(my_display, window, &window_name);

    XTextProperty icon_name;
    XStringListToTextProperty(&ICON_NAME, 1, &icon_name);
    XSetWMIconName(my_display, window, &icon_name);

    // map the windows
    XMapWindow(my_display, window);

    // enter the event loop
    bool done = false;
    XEvent event;
    while (!done) {
        XNextEvent(my_display, &event); // receive an event

        switch (event.type) { // handle the event
        case ButtonPress:
            done = true;
            break;
        }
    }

    XUnmapWindow(my_display, window);
    XDestroyWindow(my_display, window);
    XCloseDisplay(my_display);
}

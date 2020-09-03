#include <gtk/gtk.h>

static void destroy(GtkWidget* widget, gpointer data)
{
        gtk_main_quit();
}
static gboolean delete_event(GtkWidget* widget, GdkEvent *event, gpointer data)
{
        return FALSE;
}

static gboolean key_press_callback(GtkWidget *widget,
                                   GdkEventKey *event,
                                   GtkWidget *user_data)
{
        if (event->keyval == GDK_KEY_F1) {
                //gtk_label_set_text(GTK_LABEL(user_data), "My Name");
                //gtk_window_set_title(GTK_WINDOW(widget), "Deng Hongzhi");
                g_object_set(G_OBJECT(widget), "title", "Deng Hongzhi", NULL);
                g_object_set(G_OBJECT(user_data), "label", "My Name", NULL);
                return TRUE;
        }
        return FALSE;
}

static void notify_window_title_changed (GObject    *gobject,
                                  GParamSpec *pspec,
                                  gpointer    user_data)
{
        const gchar *title = gtk_window_get_title(GTK_WINDOW(gobject));
        g_message("Window title changed to %s\n", title);
}

int main(int argc, char *argv[])
{
        gtk_init(&argc, &argv);

        // top level windows got WM decorations and a border frame, managed by the WM
        GtkWidget *window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
//        gtk_window_set_title(GTK_WINDOW(window), "Hans Meyer");
        g_object_set(G_OBJECT(window), "title", "Hans Meyer", NULL);
        g_object_set(G_OBJECT(window), "border-width", 10, NULL);
        //gtk_container_set_border_width(GTK_CONTAINER(window), 10);
        gtk_window_set_resizable(GTK_WINDOW(window), FALSE);
        gtk_widget_set_size_request(window, 300, 100); // sets the minimum size of a widget

        g_signal_connect(G_OBJECT(window), "destroy", G_CALLBACK(destroy), NULL);
        g_signal_connect(G_OBJECT(window), "delete_event", G_CALLBACK(delete_event), NULL);
        g_signal_connect(G_OBJECT(window), "notify::title", G_CALLBACK(notify_window_title_changed), NULL);

        // often used to label other widgets, but also to create large blocks of
        // uneditable, formatted or wrapped text
        GtkWidget *label = gtk_label_new("Meyer");
        gtk_label_set_selectable(GTK_LABEL(label), TRUE);
        // so that the text can be selected
        // error messages shoudl be set as selectable
        g_signal_connect(G_OBJECT(window), "key-press-event",
                         G_CALLBACK(key_press_callback), label);

        // add the label as a child widget of the window
        gtk_container_add(GTK_CONTAINER(window), label);

        // GTK queues the widget until all preprocessing is complete before it
        // is drawn onto the screen. Not drawn if the parent is not drawn.
        // The children will be drawn.
        // the function recursively draws the window, its children and so on
        gtk_widget_show_all(window);

        // enter the main loop and start processing events
        gtk_main();
        return 0;
}

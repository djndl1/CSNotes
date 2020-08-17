#include <gtk/gtk.h>

static void destroy(GtkWidget* widget, gpointer data)
{
        gtk_main_quit();
}
static gboolean delete_event(GtkWidget* widget, GdkEvent *event, gpointer data)
{
        return FALSE;
}

int main(int argc, char *argv[])
{
        gtk_init(&argc, &argv);

        // top level windows got WM decorations and a border frame, managed by the WM
        GtkWidget *window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
        gtk_window_set_title(GTK_WINDOW(window), "Hello world");
        gtk_container_set_border_width(GTK_CONTAINER(window), 10);
        gtk_widget_set_size_request(window, 200, 100); // sets the minimum size of a widget

        g_signal_connect(G_OBJECT(window), "destroy", G_CALLBACK(destroy), NULL);
        g_signal_connect(G_OBJECT(window), "delete_event", G_CALLBACK(delete_event), NULL);

        // often used to label other widgets, but also to create large blocks of
        // uneditable, formatted or wrapped text
        GtkWidget *label = gtk_label_new("Hello World");
        gtk_label_set_selectable(GTK_LABEL(label), TRUE); // so that the text can be selected

        // add the label as a child widget of the window
        gtk_container_add(GTK_CONTAINER(window), label);

        // GTK queues the widget until all preprocessing is complete before it
        // is drawn onto the screen. Not drawn if the parent is not drawn.
        // The children will be drawn. 
        gtk_widget_show_all(window);

        // enter the main loop
        gtk_main();
        return 0;
}

#include <gtk/gtk.h>
#include <gdk/gdk.h>

static void destroy_app(GtkWidget *widget, gpointer *user_data)
{
    gtk_main_quit();
}


static gboolean button_pressed(GtkWidget *widget,
                               GdkEventButton* event,
                               GtkLabel* label)
{
    if (event->type == GDK_2BUTTON_PRESS)
    {
        const gchar *text = gtk_label_get_text(label);

        if (text[0] == 'D')
            gtk_label_set_text(label, "I was double-clicked");
        else
            gtk_label_set_text(label, "Double Clicke Me Again");
    }

    return FALSE;
}

int main(int argc, char *argv[])
{
    gtk_init(&argc, &argv);

    GtkWidget *window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(window), "Panes");
    gtk_container_set_border_width(GTK_CONTAINER(window), 5);
    gtk_widget_set_size_request(window, 300, 400);
    g_signal_connect(G_OBJECT(window), "destroy", G_CALLBACK(destroy_app), NULL);

    GtkWidget *eventbox = gtk_event_box_new();
    GtkWidget *label = gtk_label_new("Double-Clike Me!");

    // set the order in which widgets will receive notification events
    gtk_event_box_set_above_child(GTK_EVENT_BOX(eventbox), FALSE);

    g_signal_connect(G_OBJECT(eventbox), "button_press_event",
                     G_CALLBACK(button_pressed), (gpointer)label);


    /*
    ** allow the event box to catch button presses, realize the widget and set
    ** the cusor that will be displayd when the mouse is over the event box
     */
    gtk_container_add(GTK_CONTAINER(eventbox), label);
    gtk_container_add(GTK_CONTAINER(window), eventbox);
    gtk_widget_set_events(eventbox, GDK_BUTTON_PRESS_MASK);
    gtk_widget_realize(eventbox);
    gdk_window_set_cursor(, gdk_cursor_new(GDK_HAND1));
    
    gtk_widget_show_all(window);
    gtk_main();

    return 0;
}

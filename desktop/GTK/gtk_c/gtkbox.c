#include <gtk/gtk.h>

const gchar *names[] = {"Andrem",
                        "Joe",
                        "Samantha",
                        "Jonathan"};

static void destroy(GtkWidget *widget, gpointer user_data)
{
    gtk_main_quit();
}

int main(int argc, char *argv[])
{
    gtk_init(&argc, &argv);

    GtkWidget *window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(window), "Boxes");
    gtk_container_set_border_width(GTK_CONTAINER(window), 10);
    gtk_widget_set_size_request(window, 300, 200);

    GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    for (int i = 0; i < 4; i++ ) {
        GtkWidget *button = gtk_button_new_with_label(names[i]);
        gtk_box_pack_start(GTK_BOX(vbox), button, FALSE, TRUE, 5);
        g_signal_connect_swapped(G_OBJECT(button), "clicked",
            G_CALLBACK(gtk_widget_destroy), button);
    }

    gtk_container_add(GTK_CONTAINER(window), vbox);
    g_signal_connect(G_OBJECT(window), "destroy", G_CALLBACK(destroy), NULL);
    gtk_widget_show_all(window);

    gtk_main();
    
    return 0;
}

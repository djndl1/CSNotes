#include <gtk/gtk.h>

static void destroy_app(GtkWidget *widget, gpointer *user_data);

int main(int argc, char *argv[])
{
    gtk_init(&argc, &argv);

    GtkWidget *window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(window), "Panes");
    gtk_container_set_border_width(GTK_CONTAINER(window), 5);
    gtk_widget_set_size_request(window, 300, 400);
    g_signal_connect(G_OBJECT(window), "destroy", G_CALLBACK(destroy_app), NULL);
    
    GtkWidget *table = gtk_expander_new("Click me for more");
    
    GtkWidget *label1 = gtk_label_new("Enter the following info...");


    gtk_container_add(GTK_CONTAINER(table), label1);

    gtk_container_add(GTK_CONTAINER(window), table);

    gtk_widget_show_all(window);
    gtk_main();

    return 0;
}

static void destroy_app(GtkWidget *widget, gpointer *user_data)
{
    gtk_main_quit();
}

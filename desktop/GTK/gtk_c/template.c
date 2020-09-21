#include <gtk/gtk.h>


int main(int argc, char *argv[])
{
    gtk_init(&argc, &argv);

    GtkWidget *main_win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(main_win), "Iterator");
    gtk_widget_set_size_request(main_win, 500, 400);
    gtk_container_set_border_width(GTK_CONTAINER(main_win), 10);
    g_signal_connect(G_OBJECT(main_win), "destroy",
                     G_CALLBACK(gtk_main_quit), NULL);






    gtk_widget_show_all(main_win);

    gtk_main();


    return 0;
}

#include <gtk/gtk.h>

static gboolean pulse_progress(GtkProgressBar *bar)
{
    static gint i = 0;
    gtk_progress_bar_pulse(bar);
    i++;
    return (i < 250);
}

int main(int argc, char *argv[])
{
    gtk_init(&argc, &argv);

    GtkWidget *window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(window), "timeouts");
    gtk_container_set_border_width(GTK_CONTAINER(window), 10);
    gtk_widget_set_size_request(window, 200, 300);

    GtkWidget *progress_bar = gtk_progress_bar_new();
    gtk_progress_bar_set_pulse_step(GTK_PROGRESS_BAR(progress_bar), 0.5);
    g_timeout_add(100, (GSourceFunc)pulse_progress, progress_bar);


    gtk_container_add(GTK_CONTAINER(window), progress_bar);
    gtk_widget_show_all(window);

    gtk_main();
    return 0;
}

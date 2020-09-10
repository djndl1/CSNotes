#include <gtk/gtk.h>
#include <libintl.h>

static void dialog_button_clicked(GtkButton *button, GtkWindow *data)
{
    GtkWidget *msg_dialog = gtk_message_dialog_new(data, GTK_DIALOG_MODAL, GTK_MESSAGE_INFO,
                                                   GTK_BUTTONS_OK, "The button was clicked");
    gtk_window_set_title(GTK_WINDOW(msg_dialog), "Information");
    gtk_dialog_run(GTK_DIALOG(msg_dialog));
    gtk_widget_destroy(msg_dialog);
}

int main(int argc, char* argv[])
{
    gtk_init(&argc, &argv);

    GtkWidget *main_window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_container_set_border_width(GTK_CONTAINER(main_window), 10);
    gtk_window_set_title(GTK_WINDOW(main_window), "Simple Dialog");
    gtk_widget_set_size_request(main_window, 300, 400);
    g_signal_connect_swapped(G_OBJECT(main_window), "destroy", G_CALLBACK(gtk_main_quit), NULL);

    GtkWidget *dialog_button = gtk_button_new_with_mnemonic("_Open a dialog");
    g_signal_connect(G_OBJECT(dialog_button), "clicked",
                     G_CALLBACK(dialog_button_clicked), main_window);

    gtk_container_add(GTK_CONTAINER(main_window), dialog_button);
    gtk_widget_show_all(main_window);

    gtk_main();

    return 0;

}

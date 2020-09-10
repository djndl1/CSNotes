#include <gtk/gtk.h>
#include <libintl.h>

static void dialog_button_clicked(GtkButton *button, GtkWindow *data)
{
    GtkWidget *dialog = gtk_dialog_new_with_buttons("Information", data,
                                                    GTK_DIALOG_MODAL,
                                                    gettext("OK"), GTK_RESPONSE_OK,
                                                    NULL);


    GtkWidget *label = gtk_label_new("The button was clicked");
    GtkWidget *image = gtk_image_new_from_icon_name("dialog-information",
                                                GTK_ICON_SIZE_DIALOG);
    GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
    gtk_container_set_border_width(GTK_CONTAINER(hbox), 5);
    gtk_box_pack_start(GTK_BOX(hbox), image, TRUE, TRUE, 5);
    gtk_box_pack_start(GTK_BOX(hbox), label, TRUE, TRUE, 5);

    GtkWidget *content = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
    gtk_container_add(GTK_CONTAINER(content), hbox);
  
    gtk_widget_show_all(dialog);
 //   gtk_dialog_run(GTK_DIALOG(dialog)); // always set modal
  //  gtk_widget_destroy(dialog);

    // in order to create a nonmodal dialog, connect to the `response` dialog
    g_signal_connect(G_OBJECT(dialog), "response",
                             G_CALLBACK(gtk_widget_destroy), NULL);
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

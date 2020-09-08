#include <gtk/gtk.h>

static void destroy_app(GtkWidget *widget, gpointer *user_data);


static void switch_page(GtkButton *button,
                        GtkNotebook *notebook)
{
    gint page = gtk_notebook_get_current_page(notebook);
    if (page == 0)
        gtk_notebook_set_current_page(notebook, 1);
    else
        gtk_notebook_set_current_page(notebook, 0);
}

int main(int argc, char *argv[])
{
    gtk_init(&argc, &argv);

    GtkWidget *window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(window), "Panes");
    gtk_container_set_border_width(GTK_CONTAINER(window), 5);
    gtk_widget_set_size_request(window, 300, 400);
    g_signal_connect(G_OBJECT(window), "destroy", G_CALLBACK(destroy_app), NULL);
    
    GtkWidget *radio1 = gtk_radio_button_new_with_label(NULL, "Clicked me");
    GtkWidget *radio2 = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(radio1), "Clicked me instead");
    GtkWidget *radio3 = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(radio1),
                                                                    "No! Click me!");
    GtkWidget *radio4 = gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON(radio1),
                                                                    "Just click me!");

    GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 10);
    gtk_box_pack_start(GTK_BOX(vbox), radio1, FALSE, FALSE, 5);
    gtk_box_pack_start(GTK_BOX(vbox), radio2, FALSE, FALSE, 5);
    gtk_box_pack_start(GTK_BOX(vbox), radio3, FALSE, FALSE, 5);
    gtk_box_pack_start(GTK_BOX(vbox), radio4, FALSE, FALSE, 5);

    gtk_container_add(GTK_CONTAINER(window), vbox);

    gtk_widget_show_all(window);
    gtk_main();

    return 0;
}

static void destroy_app(GtkWidget *widget, gpointer *user_data)
{
    gtk_main_quit();
}

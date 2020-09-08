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
    
    gchar *str = g_strconcat("What is the password for ", g_get_user_name(), "?", NULL);
    GtkWidget *question = gtk_label_new(str);
    GtkWidget *label = gtk_label_new("Password: ");
    GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
    gtk_box_pack_start(GTK_BOX(hbox), question, TRUE, FALSE, 5);
    gtk_box_pack_start(GTK_BOX(hbox), label, TRUE, FALSE, 5);

    GtkWidget *pass_entry = gtk_entry_new();
    gtk_entry_set_visibility(GTK_ENTRY(pass_entry), FALSE);
    gtk_entry_set_invisible_char(GTK_ENTRY(pass_entry), '*');

    GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox),
                       hbox, TRUE, FALSE, 10);
    gtk_box_pack_start(GTK_BOX(vbox),
                       pass_entry, TRUE, FALSE, 10);


    gtk_container_add(GTK_CONTAINER(window), vbox);

    gtk_widget_show_all(window);
    gtk_main();

    return 0;
}

static void destroy_app(GtkWidget *widget, gpointer *user_data)
{
    gtk_main_quit();
}

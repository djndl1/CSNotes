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
    
    GtkWidget *notebook = gtk_notebook_new();
    GtkWidget *label1 = gtk_button_new_with_label("Page one");
    GtkWidget *label2 = gtk_button_new_with_label("Page Two");
    GtkWidget *child1 = gtk_label_new("Go to page 2 to find the answer");
    GtkWidget *child2 = gtk_label_new("go to page 1 to find the answer");

    g_signal_connect(G_OBJECT(label1), "clicked",
                     G_CALLBACK(switch_page), (gpointer) notebook);
    g_signal_connect(G_OBJECT(label2), "clicked",
                     G_CALLBACK(switch_page), (gpointer) notebook);

    gtk_notebook_append_page(GTK_NOTEBOOK(notebook), child1, label1);
    gtk_notebook_append_page(GTK_NOTEBOOK(notebook), child2, label2);
    gtk_notebook_set_tab_pos(GTK_NOTEBOOK(notebook), GTK_POS_TOP);

    gtk_container_add(GTK_CONTAINER(window), notebook);

    gtk_widget_show_all(window);
    gtk_main();

    return 0;
}

static void destroy_app(GtkWidget *widget, gpointer *user_data)
{
    gtk_main_quit();
}

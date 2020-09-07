#include <gtk/gtk.h>

static void destroy_app(GtkWidget *widget, gpointer user_data);


 void next_page(GtkWidget *button,
                        gpointer notebook)
{
    gint page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
    
    gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), (page + 1) % 4);
}
 void last_page(GtkWidget *button,
                        gpointer notebook)
{
    gint page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));

    gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), (page + 3) % 4);
}

static void just_move_page(GtkButton *button, GtkNotebook *notebook)
{
      gint page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));

    gtk_notebook_set_current_page(GTK_NOTEBOOK(notebook), (page + 3) % 4);
}

static void print_message(GtkWidget *widget, gpointer user_data)
{
    g_message("Test called\n");
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
    GtkWidget *last_button = gtk_button_new_with_label("Last");
    GtkWidget *next_button = gtk_button_new_with_label("Next");
    g_signal_connect(G_OBJECT(last_button), "clicked",
                     G_CALLBACK(last_page), (gpointer) notebook);
    g_signal_connect(G_OBJECT(next_button), "clicked",
                     G_CALLBACK(destroy_app), NULL);
    
    GtkWidget *nav_hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
    gtk_container_add(GTK_CONTAINER(nav_hbox), last_button);
    gtk_container_add(GTK_CONTAINER(nav_hbox), next_button);


    GtkWidget *child1 = gtk_button_new_with_label("Move to page 2");
    g_signal_connect(G_OBJECT(child1), "clicked", G_CALLBACK(next_page), (gpointer)notebook);
    GtkWidget *child2 = gtk_button_new_with_label("move to page 3");
    g_signal_connect(G_OBJECT(child2), "clicked", G_CALLBACK(next_page), (gpointer)notebook);
    GtkWidget *child3 = gtk_button_new_with_label("move to page 4");
    g_signal_connect(G_OBJECT(child3), "clicked", G_CALLBACK(next_page), (gpointer)notebook);
    GtkWidget *child4 = gtk_button_new_with_label("move to page 1");
    g_signal_connect(G_OBJECT(child4), "clicked", G_CALLBACK(next_page), (gpointer)notebook);



    gtk_notebook_set_show_tabs(GTK_NOTEBOOK(notebook), FALSE);
    gtk_notebook_append_page(GTK_NOTEBOOK(notebook), child1, NULL);
    gtk_notebook_append_page(GTK_NOTEBOOK(notebook), child2, NULL);
    gtk_notebook_append_page(GTK_NOTEBOOK(notebook), child3, NULL);
    gtk_notebook_append_page(GTK_NOTEBOOK(notebook), child4, NULL);
    gtk_notebook_set_tab_pos(GTK_NOTEBOOK(notebook), GTK_POS_TOP);

    GtkWidget *vbox = gtk_paned_new(GTK_ORIENTATION_VERTICAL);
    gtk_paned_add1(GTK_PANED(vbox), notebook);
    gtk_paned_add2(GTK_PANED(vbox), nav_hbox);

    gtk_container_add(GTK_CONTAINER(window), vbox);

    gtk_widget_show_all(window);
    gtk_main();

    return 0;
}

static void destroy_app(GtkWidget *widget, gpointer user_data)
{
    gtk_main_quit();
}

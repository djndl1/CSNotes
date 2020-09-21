#include <gtk/gtk.h>


static gint cnt = 0;

static void reset_button_clicked(GtkButton *reset_button, GtkLabel *count_label)
{
    cnt = 0;
    gtk_label_set_text(count_label, "0");
}

static gboolean increment_count(gpointer label)
{
    cnt++;
    GString *cnt_str = g_string_new("");
    g_string_printf(cnt_str, "%d", cnt);

    label  = (GtkLabel *) label;
    gtk_label_set_text(label, cnt_str->str);

    return TRUE;
}

int main(int argc, char *argv[])
{
    gtk_init(&argc, &argv);

    GtkWidget *main_win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(main_win), "Timout count");
    gtk_widget_set_size_request(main_win, 300, 400);
    g_signal_connect_swapped(G_OBJECT(main_win), "destroy",
        G_CALLBACK(gtk_main_quit), NULL);


    GtkWidget *count_label = gtk_label_new("0");
    GtkWidget *reset_button = gtk_button_new_with_label("reset");
    g_signal_connect(G_OBJECT(reset_button), "clicked",
                     G_CALLBACK(reset_button_clicked), count_label);

    GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox), count_label, FALSE, FALSE, 5);
    gtk_box_pack_start(GTK_BOX(vbox), reset_button, FALSE, FALSE, 5);

    gtk_container_add(GTK_CONTAINER(main_win), vbox);

    g_timeout_add(1000, increment_count, count_label);

    gtk_widget_show_all(main_win);
    gtk_main();

    return 0;
}

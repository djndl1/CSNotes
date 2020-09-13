#include <gtk/gtk.h>

typedef struct {
    GtkWidget *content;
    GtkWidget *file_chooser;
} write_data;

static void write_button_clicked(GtkButton *button, write_data *data)
{
    gchar *folder_path = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(data->file_chooser));
    gchar const *text = gtk_entry_get_text(GTK_ENTRY(data->content));
    gchar *filename = g_uuid_string_random();
    gchar *filepath = g_build_filename(folder_path, filename, NULL);

    GError *err = NULL;
    g_print("Writing '%s' to %s\n", text, filepath);
    gboolean status = g_file_set_contents(filepath, text, -1, &err);
    if (!status)
        g_warning("Failed to write a file\n");
    gboolean exists = g_file_test(filepath,
                                  G_FILE_TEST_EXISTS);
    if (!exists) {
        g_warning("The file is not there\n");
    }
}

int main(int argc, char *argv[])
{
    gtk_init(&argc, &argv);

    GtkWidget *main_win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(main_win), "Working with files");
    gtk_widget_set_size_request(main_win, 300, 200);
    g_signal_connect_swapped(G_OBJECT(main_win), "destroy",
                             G_CALLBACK(gtk_main_quit), NULL);

    GtkWidget *entry = gtk_entry_new();
    GtkWidget *chooser = gtk_file_chooser_button_new("Choose a file",
                                                     GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER);
    write_data combined = {
        .content = entry,
        .file_chooser = chooser
    };

    GtkWidget *write_button = gtk_button_new_with_label("Write the file");
    g_signal_connect(G_OBJECT(write_button), "clicked",
                     G_CALLBACK(write_button_clicked), &combined);

    GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox), entry, FALSE, FALSE, 5);
    gtk_box_pack_start(GTK_BOX(vbox), chooser, FALSE, FALSE, 5);
    gtk_box_pack_start(GTK_BOX(vbox), write_button, FALSE, FALSE, 5);
    gtk_container_add(GTK_CONTAINER(main_win), vbox);

    gtk_widget_show_all(main_win);

    gtk_main();

    return 0;
}

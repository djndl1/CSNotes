#include <gtk/gtk.h>

typedef struct {
    GtkWidget *entry, *textview;
} Widgets;

static void insert_text(GtkButton *, Widgets *);
static void retrieve_text(GtkButton *, Widgets *);

int main(int argc, char *argv[])
{
    gtk_init(&argc, &argv);

    GtkWidget *main_win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(main_win), "Iterator");
    gtk_widget_set_size_request(main_win, 500, 400);
    gtk_container_set_border_width(GTK_CONTAINER(main_win), 10);
    g_signal_connect(G_OBJECT(main_win), "destroy",
                     G_CALLBACK(gtk_main_quit), NULL);

    Widgets *w = g_slice_new(Widgets);
    w->textview = gtk_text_view_new();
    w->entry = gtk_entry_new();

    GtkWidget *insert = gtk_button_new_with_label("Insert Text");
    g_signal_connect(G_OBJECT(insert), "clicked",
        G_CALLBACK(insert_text), (gpointer) w);

    GtkWidget *retrieve = gtk_button_new_with_label("Get Text");
    g_signal_connect(G_OBJECT(retrieve), "clicked",
        G_CALLBACK(retrieve_text), (gpointer) w);

    GtkWidget *scrolled_win = gtk_scrolled_window_new(NULL, NULL);
    gtk_container_add(GTK_CONTAINER(scrolled_win), w->textview);

    GtkWidget *hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 5);
    gtk_box_pack_start(GTK_BOX(hbox), w->entry, TRUE, TRUE, 3);
    gtk_box_pack_start(GTK_BOX(hbox), insert, TRUE, TRUE, 3);
    gtk_box_pack_start(GTK_BOX(hbox), retrieve, TRUE, TRUE, 3);

    GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
    gtk_box_pack_start(GTK_BOX(vbox), scrolled_win, TRUE, TRUE, 3);
    gtk_box_pack_end(GTK_BOX(vbox), hbox, FALSE, FALSE, 3);

    gtk_container_add(GTK_CONTAINER(main_win), vbox);
    gtk_widget_show_all(main_win);

    gtk_main();


    return 0;
}

static void insert_text(GtkButton *button, Widgets *w)
{
    GtkTextBuffer *buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(w->textview));
    const gchar *text = gtk_entry_get_text(GTK_ENTRY(w->entry));

    GtkTextMark *mark = gtk_text_buffer_get_insert(buffer);

    GtkTextIter iter;
    gtk_text_buffer_get_iter_at_mark(buffer, &iter, mark);
    gtk_text_buffer_insert(buffer, &iter, text, -1);
}

static void retrieve_text(GtkButton *button, Widgets *w)
{
    GtkTextBuffer *buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(w->textview));

    GtkTextIter start, end;
    gtk_text_buffer_get_selection_bounds(buffer, &start, &end);
    gchar *text = gtk_text_buffer_get_text(buffer, &start, &end, FALSE);

    g_print("%s\n", text);
}

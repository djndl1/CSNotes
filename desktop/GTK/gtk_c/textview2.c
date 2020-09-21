#include <gtk/gtk.h>


int main(int argc, char *argv[])
{
    gtk_init(&argc, &argv);

    GtkWidget *main_win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_container_set_border_width(GTK_CONTAINER(main_win), 10);
    gtk_widget_set_size_request(main_win, 400, 500);
    gtk_window_set_title(GTK_WINDOW(main_win), "TextView 2");

    PangoFontDescription *font = pango_font_description_from_string("Fira Code 10");
    GtkWidget *textview = gtk_text_view_new();
    gtk_widget_override_font(textview, font);

    gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(textview), GTK_WRAP_WORD);
    gtk_text_view_set_justification(GTK_TEXT_VIEW(textview), GTK_JUSTIFY_LEFT);
    gtk_text_view_set_editable(GTK_TEXT_VIEW(textview), TRUE);
    gtk_text_view_set_cursor_visible(GTK_TEXT_VIEW(textview), TRUE);

    gtk_text_view_set_pixels_above_lines(GTK_TEXT_VIEW(textview), 5);
    gtk_text_view_set_pixels_below_lines(GTK_TEXT_VIEW(textview), 5);
    gtk_text_view_set_pixels_inside_wrap(GTK_TEXT_VIEW(textview), 5);

    gtk_text_view_set_left_margin(GTK_TEXT_VIEW(textview), 10);
    gtk_text_view_set_right_margin(GTK_TEXT_VIEW(textview), 10);

    GtkTextBuffer *buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(textview));
    gtk_text_buffer_set_text(buffer, "There is some text!\nChange me!\nPlease", -1);

    GtkWidget *scrolled_win = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_win), GTK_POLICY_AUTOMATIC, GTK_POLICY_ALWAYS);

    gtk_container_add(GTK_CONTAINER(scrolled_win), textview);
    gtk_container_add(GTK_CONTAINER(main_win), scrolled_win);
    gtk_widget_show_all(main_win);

    gtk_main();
    return 0;
}

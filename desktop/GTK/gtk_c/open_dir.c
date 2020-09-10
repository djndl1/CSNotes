#include <glib.h>

int main()
{
GDir *dir = g_dir_open(g_get_home_dir(), 0, NULL);

if (!g_file_test(g_get_home_dir(), G_FILE_TEST_IS_DIR))
    g_error("You don't have a home directory!");

const gchar *file;
while ((file = g_dir_read_name(dir)))
    g_print("%s\n", file);

g_dir_close(dir);

return 0;
}

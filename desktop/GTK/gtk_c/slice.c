#include <glib.h>


int main()
{
    gchar *strings[100];

    // internally, GLib decides whether to us slab allocation or
    // delegate the memory allocation to `g_malloc`. Memory allocation is performed by
    // `g_malloc()` when the desired memory slice is very large.
    for (int i = 0; i < 100; i++) {
        strings[i] = g_slice_alloc(10);
        strings[i] = g_strdup("Ten bytes");
    }

    for (int i = 0; i < 100; i++)
        g_print("%s\n", strings[i]);


    for (int i = 0; i < 100; i++) {
        g_slice_free1(10, strings[i]);
    }

    // use `g_new()` for multiple objects.
    GSList *w = g_slice_new(GSList);
    g_slice_free(GSList, w);
}

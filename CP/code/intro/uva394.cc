#include <cstdio>
#include <cstdint>
#include <cstdlib>
#include <cstring>

struct _bounds {
    unsigned lower_bounds;
    unsigned upper_bounds;
};

typedef struct _arrayInfo {
    char name[15];
    unsigned base;
    unsigned elem_size;
    unsigned ndims;

    struct _bounds bounds[10];
} array_info;

const array_info* get_array_info(const array_info *infos,
                                 const int info_num,
                                 const char* array_name)
{
    for (int i = 0; i < info_num; i++) {
        if (strcmp(infos[i].name, array_name) == 0)
            return &infos[i];
    }
    return NULL;
}

unsigned compute_address(const array_info *info, const unsigned *indices)
{
    unsigned addr = info->base;

    int i = info->ndims - 1;
    unsigned ind_size = info->elem_size;
    while (i >= 0) {
        addr += ind_size * (indices[i] - info->bounds[i].lower_bounds);
        ind_size *= (info->bounds[i].upper_bounds - info->bounds[i].lower_bounds + 1);
        i--;
    }

    return addr;
}


int main()
{
    int N, R;
    scanf("%d %d\n", &N, &R);

    array_info *arrays = (array_info*) malloc(sizeof(array_info) * N);
    for (int i = 0; i < N; i++) {
        scanf("%s", arrays[i].name);
        scanf("%u %u %u",
              &arrays[i].base, &arrays[i].elem_size, &arrays[i].ndims);

        for (int j = 0; j < arrays[i].ndims; j++) {
            scanf("%u %u",
                  &arrays[i].bounds[j].lower_bounds,
                  &arrays[i].bounds[j].upper_bounds);
        }
        getchar();
    }

    while (R--) {
        char arr_name[15];
        scanf("%s", arr_name);
        const array_info *this_array = get_array_info(arrays, N, arr_name);
        unsigned indices[10] = { 0 };
        for (int i = 0; i < this_array->ndims; i++) {
            scanf("%d", &indices[i]);
        }
        getchar();

        printf("%s[", this_array->name);
        for (int i = 0; i < this_array->ndims; i++) {
            if (i > 0)
                printf(", ");
            printf("%d", indices[i]);
        }
        printf("] = %u\n", compute_address(this_array, indices));

    }
}

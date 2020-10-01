#include <cstdio>


const char* blown = "Fuse was blown.";
const char *not_blown = "Fuse was not blown.";

int main()
{
    int dev_num, op_num, capacity;

    int i = 1;
    while (scanf("%d %d %d\n", &dev_num, &op_num, &capacity),
           (dev_num || op_num || capacity)) {
        int dev_loads[20] = {0};
        for (int di = 0; di < dev_num; di++) {
            scanf("%d", &dev_loads[di]);
        }

        bool dev_statuses[20] = { false };
        int load_current = 0;
        int highest_load = 0;
        while (op_num--) {
            int dev;
            scanf("%d", &dev);
            dev_statuses[dev-1] = !dev_statuses[dev-1];

            if (dev_statuses[dev-1])
                load_current += dev_loads[dev-1];
            else
                load_current -= dev_loads[dev-1];

            highest_load = load_current > highest_load ? load_current : highest_load;
        }

        printf("Sequence %d\n", i);
        if (highest_load > capacity)
            printf("%s\n", blown);
        else {
            printf("%s\n"
                   "Maximal power consumption was %d amperes.\n",
                   not_blown, highest_load);
        }
        putchar('\n');

        i++;
    }

}

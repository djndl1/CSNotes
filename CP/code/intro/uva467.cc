#include <cstdio>
#include <cstring>

int get_ref(int *intset, size_t len)
{
    int refint = intset[0];
    for (size_t i = 0; i < len; i++) {
        if (intset[i] < refint)
            refint = intset[i];
    }

    return refint;
}

bool is_green_during(int lower, int upper, int cycle)
{

    int cursor = 2 * cycle;
    int cursor_limit = cursor + cycle - 5;
    while (cursor < upper) {
        if (cursor < lower && cursor_limit > lower)
            return true;
        if (cursor >= lower)
            return true;

        cursor += 2 * cycle;
        cursor_limit = cursor + cycle - 5;
    }

    return false;
}

int find_simul_green(int *dataset, size_t signal_num)
{
    int ref_cycle = get_ref(dataset, signal_num);

    int ref_signal = ref_cycle * 2;
    while (ref_signal < 3600) {
        int lower_bound = ref_signal;
        int upper_bound = ref_signal - 5 + ref_cycle;

        bool greened[10] = {false};

        for (size_t i = 0; i < signal_num; i++) {
            greened[i] = is_green_during(lower_bound, upper_bound, dataset[i]);
        }

        bool all_green = true;
        for (size_t i = 0; i < signal_num; i++) {
            all_green = all_green && greened[i];
        }
        if (all_green)
            return ref_signal;

        ref_signal += 2 * ref_cycle;
    }

    return -1;
}

int main()
{
    int case_num = 1;

    int sig_secs;
    char temp_line[50];
    while (fgets(temp_line, 50, stdin) != NULL) {
        int dataset[10] = {0};
        size_t signal_num = 0;

        char *token = strtok(temp_line, " ");
        while (token) {
            int secs;
            sscanf(token, "%d", &secs);
            dataset[signal_num++] = secs;

            token = strtok(NULL , " ");
        }
        int green_time = find_simul_green(dataset, signal_num);
        printf("green_time %d\n", green_time);
        if (green_time == -1)
            printf("Set %d is unable to synch after one hour\n", case_num);
        else
            printf("Set %d synchs again at %d minute(s) and %d seconds "
                   "after all turning green\n",
                   case_num, green_time / 60, green_time % 60);

        case_num++;
    }
}

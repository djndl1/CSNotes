#include <cstdio>

int main(int argc, char *argv[])
{
    unsigned case_cnt;
    scanf("%u", &case_cnt);

    for (size_t cas = 0; cas < case_cnt; cas++) {
        unsigned wall_cnt, high_jumps = 0, low_jumps = 0;
        unsigned cur_wall, prev_wall;
        scanf("%u", &wall_cnt);

        scanf("%u", &prev_wall);
        for (size_t i = 1; i < wall_cnt; i++) {
            scanf("%u", &cur_wall);
            if (cur_wall > prev_wall)
                high_jumps++;
            else if (cur_wall < prev_wall)
                low_jumps++;

            prev_wall = cur_wall;
        }
        printf("Case %zu: %u %u\n", cas, high_jumps, low_jumps);
    }

    return 0;
}

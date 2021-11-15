#include <iostream>
#include <cstdio>
#include <vector>

int main(int argc, char *argv[])
{
    int case_cnt;
    ::scanf("%d\n", &case_cnt);

    for (int cas = 1; cas <= case_cnt; cas++) {
        int wall_cnt;
        ::scanf("%d\n", &wall_cnt);


        int wall_height_prior;
        if (wall_cnt <= 1)
        {
            printf("Case %d: 0 0\n", cas);
            if (wall_cnt == 1)
            {
                ::scanf("%d", &wall_height_prior);
            }
            continue;
        }

        ::scanf("%d", &wall_height_prior);

        int high_jump_cnt = 0;
        int low_jump_cnt = 0;
        while (--wall_cnt) {
            int wall_height_post;
            ::scanf("%d", &wall_height_post);
            if (wall_height_post > wall_height_prior)
            {
                high_jump_cnt++;
            }
            else if (wall_height_post < wall_height_prior)
            {
                low_jump_cnt++;
            }
            wall_height_prior = wall_height_post;
        }
        printf("Case %d: %d %d\n",
               cas, high_jump_cnt, low_jump_cnt);
    }

    return 0;
}

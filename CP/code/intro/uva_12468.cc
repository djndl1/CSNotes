#include <cstdio>
#include <cstdlib>

int main(int argc, char *argv[])
{
    int cur, target;
    while (scanf("%d %d", &cur, &target), (cur != -1 || target != -1)) {
        int min = cur > target ? target : cur;
        int max = cur > target ? cur : target;

        int between_diff = max - min;
        int round_diff = min + (100 - max);
        printf("%d\n", between_diff > round_diff ? round_diff : between_diff);
    }
    return 0;
}

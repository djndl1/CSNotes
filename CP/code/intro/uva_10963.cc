#include <cstdio>

int main(int argc, char *argv[])
{
    int cases;

    scanf("%d", &cases);
    while (cases--) {
        int W, y1, y2, prev_diff;
        bool result = true;
        scanf("%d", &W);
        scanf("%d %d", &y1, &y2);
        prev_diff = y1 - y2;
        while (--W) {
            scanf("%d %d", &y1, &y2);
            if (prev_diff != y1 - y2) {
                printf("no\n");
                if (cases != 0)
                    putchar('\n');
                result = false;
                break;
            }
            prev_diff = y1 - y2;
        }
        if (not result)
            continue;
        else {
          printf("yes\n");
          if (cases != 0)
            putchar('\n');
        }
    }
    return 0;
}

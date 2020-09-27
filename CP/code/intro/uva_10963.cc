#include <cstdio>

int main(int argc, char *argv[])
{
    int cases;

    scanf("%d", &cases);
    while (cases--) {
        int col_count;
        scanf("%d", &col_count);

        bool result = true;

        int northmost, southmost;
        int diff = -1;
        while (col_count--) {
            scanf("%d%d", &northmost, &southmost);
            if (result) {
                if (diff == -1)
                    diff = northmost - southmost;
                else if (diff != (northmost - southmost))
                    result = false;
            }
        }
        printf(result ? "yes\n" : "no\n");
        if (cases != 0) {
            putchar('\n');
        }
    }
    return 0;
}

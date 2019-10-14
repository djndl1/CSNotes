#include <cstdio>

int main(int argc, char *argv[])
{
    int i = 0;
    while (true) {
        int n;

        scanf("%d", &n);
        if (n == 0)
            return 0;

        int reasons = 0;
        int treats = 0;
        while (n-- > 0) {
            int tmp;
            scanf("%d", &tmp);
            if (tmp == 0)
                treats++;
            else if (tmp > 0 && tmp < 100)
                reasons++;
        }
        printf("Case %d: %d\n", ++i, reasons - treats);
    }
    return 0;
}

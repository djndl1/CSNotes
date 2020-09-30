#include <stdbool.h>
#include <stdio.h>

int main(int argc, char *argv[])
{
    int T;
    scanf("%d\n", &T);

    for (int t = 1; t <= T; t++) {
        int N;
        scanf("%d\n", &N);

        int mile = 0, juice = 0;
        while (N--) {
            int secs;
            scanf("%d", &secs);
            mile += (secs / 30 + 1) * 10;
            juice += (secs / 60 + 1) * 15;
        }
        if (mile < juice)
            printf("Case %d: Mile %d\n", t, mile);
        else if (mile > juice)
            printf("Case %d: %s %d\n", t, "Juice", juice);
        else
            printf("Case %d: %s %d\n", t, "Mile Juice", juice);
    }

    return 0;
}

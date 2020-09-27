#include <cstdio>

int main()
{
    int T;
    scanf("%d", &T);

    const int origT = T;
    while (T--) {
        int N;
        scanf("%d", &N);
        int speed = 0;
        while (N--) {
            int c;
            scanf("%d", &c);
            if (c > speed)
                speed = c;
        }

        printf("Case %d: %d\n", origT - T, speed);
    }
}
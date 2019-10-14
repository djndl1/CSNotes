#include <cstdio>

int main(int argc, char *argv[])
{
    int T, i = 0;
    scanf("%d", &T);

    while (T-- > 0) {
        int L, W, H;

        scanf("%d %d %d", &L, &W, &H);
        if (L <= 20 && W <=20 && H <= 20)
            printf("Case %d: good\n", ++i);
        else
            printf("Case %d: bad\n", ++i);
    }
    return 0;
}

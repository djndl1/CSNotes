#include <cstdio>

int main(int argc, char *argv[])
{
    long m, n, k;

    scanf("%ld", &k);

    while (k-- > 0) {
        scanf("%ld %ld", &n, &m);
        printf("%ld\n", ((n/3) * (m/3)));
    }
    return 0;
}

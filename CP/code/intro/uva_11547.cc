#include <cstdio>

using namespace std;

int main(int argc, char *argv[])
{
    int t;

    scanf("%d", &t);

    long n;
    while (t-- > 0) {
        scanf("%ld", &n);
        n = n * 567;
        n /= 9;
        n += 7492;
        n *= 235;
        n /= 47;
        n -= 498;

        if (n >= 10) {
            printf("%ld\n", (n / 10) % 10);
        } else if (n <= -10)
            printf("%ld\n", (-n / 10) % 10);
        else
            printf("0\n");
    }
}


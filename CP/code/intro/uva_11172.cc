#include <cstdio>

using namespace std;

int main(int argc, char *argv[])
{
    int n;
    scanf("%d", &n);

    long a, b;
    while (n-- > 0) {
        scanf("%ld %ld", &a, &b);
        printf("%c\n", (a == b) ? '=' : ((a > b) ? '>' : '<'));
    }

    return 0;
}

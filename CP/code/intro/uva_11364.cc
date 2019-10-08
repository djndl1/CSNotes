#include <cstdio>

using namespace std;

int main(int argc, char *argv[])
{
    int t, n;

    scanf("%d", &t);
    while (t-- > 0) {
        scanf("%d", &n);
        int max = 0, min = 100;
        int tmp;
        while (n-- > 0) {
            scanf("%d", &tmp);
            if (tmp > max) max = tmp;
            if (tmp < min) min = tmp;
        }
        printf("%d\n", 2 * (max - min));
    }
    return 0;
}


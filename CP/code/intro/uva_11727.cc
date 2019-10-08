#include <cstdio>

using namespace std;


int main(int argc, char *argv[])
{
    int T;
    scanf("%d", &T);
    

    int i = 0;
    while (T-- > 0) {
        int a, b, c;
        scanf("%d %d %d", &a, &b, &c);

        int mid;
        if (a > b && a > c && b > c)
            mid = b;
        else if (a > b && a > c && c > b)
            mid = c;
        else if (b > a && b > c && a > c)
            mid = a;
        else if (b > a && b > c && c > a)
            mid = c;
        else if (c > a && c > b && a > b)
            mid = a;
        else
            mid = b;o

        printf("Case %d: %d\n", ++i, mid);

    }
    return 0;
}

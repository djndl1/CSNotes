#include <cstdio>

int main(int argc, char *argv[])
{
    int cur, first, second, third;

    
    while (scanf("%d %d %d %d", &cur, &first, &second, &third),
           (cur || first || second || third)) {
        printf("%d\n", 1080 + ((cur + 40 - first) % 40 +
                               (second + 40 - first) % 40 +
                               (second + 40 - third) % 40) * 9);
    }
    return 0;
}

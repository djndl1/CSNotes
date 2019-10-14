#include <cstring>
#include <cstdio>

int main(int argc, char *argv[])
{
    int T;
    scanf("%d", &T);

    long sum = 0;
    char op[10];
    while (T-- > 0) {
        scanf("%s", op);
        if (strcmp(op, "donate") == 0) {
            int don;
            scanf("%d", &don);
            sum += don;
        } else
            printf("%ld\n", sum);
    }
    return 0;
}

#include <cstdio>
#include <cstring>

int main(int argc, char *argv[])
{
    char str[10];

    int i = 0;
    while (scanf("%s", str)) {
        if (str[0] == '*')
            return 0;

        if (strcmp(str, "Hajj") == 0)
            printf("Case %d: Hajj-e-Akbar\n", ++i);
        else
            printf("Case %d: Hajj-e-Asghar\n", ++i);
    }
    return 0;
}

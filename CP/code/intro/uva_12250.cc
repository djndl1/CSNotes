#include <cstdio>
#include <cstring>

int main(int argc, char *argv[])
{
    char tmp[20];

    int i = 0;
    while (scanf("%s", tmp)) {
        if (tmp[0] == '#')
            return 0;

        i++;
        if (tmp[0] != 'H' && tmp[0] != 'B' && tmp[0] != 'C' && tmp[0] != 'Z') {
            printf("Case %d: UNKNOWN\n", i);
            continue;
        }
            
            
        if (strcmp(tmp, "HELLO") == 0)
            printf("Case %d: ENGLISH\n", i);
        else if (strcmp(tmp, "HOLA") == 0)
            printf("Case %d: SPANISH\n", i);
        else if (strcmp(tmp, "HALLO") == 0)
            printf("Case %d: GERMAN\n", i);
        else if (strcmp(tmp, "BONJOUR") == 0)
            printf("Case %d: FRENCH\n", i);
        else if (strcmp(tmp, "CIAO") == 0)
            printf("Case %d: ITALIAN\n", i);
        else if (strcmp(tmp, "ZDRAVSTVUJTE") == 0)
            printf("Case %d: RUSSIAN\n", i);
        else
            printf("Case %d: UNKNOWN\n", i);
    }
    return 0;
}

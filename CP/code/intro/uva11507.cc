#include <cstdio>
#include <cstring>


typedef enum {
X_PLUS,
X_MINUS,
Y_PLUS,
Y_MINUS,
Z_PLUS,
Z_MINUS,
ORIG,
} direction_t;

direction_t parse_direction(const char *direc_string)
{
    if (strcmp(direc_string, "+x") == 0)
        return X_PLUS;
    else if (strcmp(direc_string, "-x") == 0)
        return X_MINUS;
    else if (strcmp(direc_string, "+y") == 0)
        return Y_PLUS;
    else if (strcmp(direc_string, "-y") == 0)
        return Y_MINUS;
    else if (strcmp(direc_string, "+z") == 0)
        return Z_PLUS;
    else if (strcmp(direc_string, "-z") == 0)
        return Z_MINUS;
    else
        return ORIG;
}



int main()
{
    long L;
    while (scanf("%ld\n", &L), L) {
        char direction[5];

        direction_t rel_direc = X_PLUS;
        direction_t solid_direc = X_PLUS;
        while (--L) {
            scanf("%s", direction);
            direction_t bend_direc = parse_direction(direction);
        }
    }
}

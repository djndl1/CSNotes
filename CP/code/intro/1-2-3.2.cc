#include <cstdio>
#include <cmath>

#define PI 3.141592653589793

int main(int argc, char *argv[])
{
    int precision;
    scanf("%d", &precision);

    printf("%.*lf", precision, M_PI);
    return 0;
}

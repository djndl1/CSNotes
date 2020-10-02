#include <cstdio>
#include <cctype>
#include <cstring>

int roll_digits(int n)
{
    int sum = 0;
    while (n) {
        sum += n % 10;
        n = (n - n % 10) / 10;
    }

    return sum;
}

int love_value(const char *name)
{
    int value = 0;
    char *cur = (char*)name;
    while (*cur != '\0') {
        if (isalpha(*cur)) {
            char alphbet = tolower(*cur);
            value += alphbet - 'a' + 1;
        }

        cur++;
    }
    int love_value = value;

    while (love_value >= 10) {
        love_value = roll_digits(love_value);
    }


    return love_value;
}

double love_percent(const char* a, const char *b)
{
    int a_value = love_value(a);
    int b_value = love_value(b);

    if (a_value == b_value)
        return 100;
    else if (a_value > b_value)
        return (b_value * 100.0) / a_value;
    else
        return (a_value * 100.0) / b_value;
}

int main()
{
//    printf("%d %d\n", love_value("aadsfdmmkxvczp[we"), love_value("EE"));
 //   return 0;
    while (true) {
        char a[30];
        char b[30];

        char *ares = fgets(a, 30, stdin);
        char *bres = fgets(b, 30, stdin);
        if (ares == NULL || bres == NULL)
            return 0;

        ares[strlen(ares)-1] = '\0';
        bres[strlen(bres)-1] = '\0';

        printf("%.2lf %", love_percent(ares, bres));
        putchar('\n');
    }

}

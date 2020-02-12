#include <cstdio>

int makeItToSingleDigit(unsigned long n)
{
    unsigned long sum = 0;
    while (n > 0) {
        unsigned long re = n % 10;
        n = n / 10;
        sum += re;
    }
    if (sum < 10)
        return sum;
    else
        return makeItToSingleDigit(sum);
}

int main(int argc, char *argv[])
{
    unsigned long n;

    scanf("%lud", &n);
    while (n != 0) {
        auto res = makeItToSingleDigit(n);
        printf("%u\n", res);
        scanf("%lud", &n);
    }

    return 0;
}

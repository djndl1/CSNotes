#include <bits/stdc++.h>

using namespace std;


int main(int argc, char *argv[])
{
    int N, tests = 1;

    while (scanf("%d\n", &N), N) {
        bool hasSolution = false;

        if (tests > 1)
          putchar('\n');
        for (int fghij = 01234; fghij <= 98765 / N; fghij++) {
            int abcde = fghij * N;
            int used = fghij < 10000; // flag 0
            int tmp;

            tmp = abcde;
            while (tmp) {
                used |= 1 << (tmp % 10);
                tmp /= 10;
            }
            tmp = fghij;
            while (tmp) {
                used |= 1 << (tmp % 10);
                tmp /= 10;
            }
            if (used == (1 << 10) - 1) {
                printf("%0.5d / %0.5d = %d\n", abcde, fghij, N);
                hasSolution = true;
            }
        }
        
        tests++;
        if (not hasSolution)
            printf("There are no solutions for %d.\n", N);
    }
    return 0;
}

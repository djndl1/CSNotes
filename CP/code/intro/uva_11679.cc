#include <cstdio>



int main(int argc, char *argv[])
{
    int B, N;

    int reserves[21];
    while (scanf("%d %d", &B, &N), (B || N)) {
        for (size_t i = 1; i <= B; i++) {
            int reserv;
            scanf("%d", &reserv);
            reserves[i] = reserv;
        }
        bool bailout = false;
        for (size_t i = 1; i<= N; i++) {
            int debtor, creditor, value;
            scanf("%d %d %d", &debtor, &creditor, &value);
            reserves[debtor] -= value;
            reserves[creditor] += value;
        }
        for (size_t i = 1; i <= B; i++) {
            if (reserves[i] < 0)
                bailout = true;
        }
        if (bailout)
            printf("N\n");
        else
            printf("S\n");
    }
    return 0;
}

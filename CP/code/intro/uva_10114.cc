#include <cstdio>

int main(int argc, char *argv[])
{
    int duration, deprec_num;
    double down, loan;
    double rates[101];
    while (scanf("%d %lf %lf %d", &duration, &down, &loan, &deprec_num)) {
        if (duration < 0)
            return 0;

        int month;
        double rate;
        while (deprec_num-- > 0) {
            scanf("%d %lf", &month, &rate);

            for (int i = month; i <= 100; i++)
                rates[i] = rate;
        }

        double monthly_payment = loan / duration;
        double value = (loan + down) * (1 - rates[0]);

        int i = 0;
        while (value < loan) {
            i++;
            value *= (1 - rates[i]);
            loan -= monthly_payment;
        }

        printf("%d %s\n", i, i == 1 ? "month" : "months");
    }
    return 0;
}


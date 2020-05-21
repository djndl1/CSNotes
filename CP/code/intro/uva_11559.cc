#include <cstdio>

int main(int argc, char* argv[])
{
    unsigned participants, budget, hotels, weeks;

    while (scanf("%u %u %u %u",
               &participants, &budget, &hotels, &weeks)
        != EOF) {

        unsigned minimal = 200 * 10000;

        for (int i = 0; i < hotels; i++) {
            unsigned price;
            bool hasBed = false;
            scanf("%u", &price);

            // if there are enough beds during any of the weeks
            for (int j = 0; j < weeks; j++) {
                unsigned beds;
                scanf("%u", &beds);

                if (beds < participants) 
                    continue;
                else {
                    hasBed = true;
                }
            }
            if (hasBed) {
                unsigned expense = participants * price;
                if (expense < minimal)
                     minimal = expense;
            }
        }
        if (minimal <= budget)
            printf("%u\n", minimal);
        else
            printf("stay home\n");
    }
    return 0;
}

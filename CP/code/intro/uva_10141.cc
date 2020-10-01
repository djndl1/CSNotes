#include <cstdio>
#include <cstring>
#include <cstdlib>

const size_t name_len = 81;

typedef struct _proposal_info {
    char proposal_name[name_len];
    int compliance;
    double price;
} proposal;

int main()
{
    int req_num, proposal_num;
    char garbage_text[name_len];

    int RFP = 1;
    while (scanf("%d %d\n", &req_num, &proposal_num),
           (req_num || proposal_num)) {
        for (int rq = 0; rq < req_num; rq++) {
            fgets(garbage_text, name_len, stdin);
        }

        proposal *props = (proposal*)malloc(sizeof(proposal) * proposal_num);
        int highest_compliance = 0;
        double lowest_price = 0;
        char *optim_prop_name;
        for (int pr = 0; pr < proposal_num; pr++) {
            char *prop_name = props[pr].proposal_name;
            fgets(prop_name, name_len, stdin);
            scanf("%lf %d\n", &props[pr].price, &props[pr].compliance);

            if (props[pr].compliance > highest_compliance) {
                optim_prop_name = prop_name;
                highest_compliance = props[pr].compliance;
                lowest_price = props[pr].price;
            }

            for (int rq = 0; rq < props[pr].compliance; rq++) {
                fgets(garbage_text, name_len, stdin);
            }
        }

        for (int pr = 0; pr < proposal_num; pr++) {
            if (props[pr].compliance == highest_compliance &&
                props[pr].price < lowest_price) {
                lowest_price = props[pr].price;
                optim_prop_name = props[pr].proposal_name;
            }
        }

        if (RFP != 1)
            putchar('\n');

        printf("RFP #%d\n"
               "%s",
               RFP, optim_prop_name);

        RFP++;
    }
}

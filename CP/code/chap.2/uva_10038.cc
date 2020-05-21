#include <bits/stdc++.h>

using namespace std;

int main(int argc, char *argv[])
{
    int num;
    while (scanf("%d", &num) != EOF) {
        vector<bool> jolly_seq(num-1, false);
        bool isJolly = true;
        int prev, curr;

        scanf("%d", &prev);
        for (size_t i = 1; i < num; i++) {
            scanf("%d", &curr);
            int diff = ::abs(curr-prev);
            if ((diff >= 1) && (diff <= num-1))
                jolly_seq[diff-1] = true;
            prev = curr;
        }

        for (auto b : jolly_seq) {
            //std::cout << b << " ";
            if (not b) {
                isJolly = false;
                break;
            }
        }

        if (isJolly)
            printf("Jolly\n");
        else
            printf("Not jolly\n");
    }
    return 0;
}


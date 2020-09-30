#include <cstdio>
#include <climits>

int main()
{

    int T;
    scanf("%d", &T);

    for (int t = 1; t <= T; t++) {
        char cur[10][101];
        int relevances[10] = {0};
        int highest = INT_MIN;

        for (int curUrl = 0; curUrl < 10; curUrl++) {
            scanf("%s %d", cur[curUrl], &relevances[curUrl]);
            highest = highest < relevances[curUrl] ? relevances[curUrl] : highest;
        }

        printf("Case #%d:\n", t);
        for (int i = 0; i < 10; i++) {
            if (relevances[i] == highest) {
                printf("%s\n", cur[i]);
            }
        }
    }
}

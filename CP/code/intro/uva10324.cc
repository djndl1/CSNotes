#include <cstdio>
#include <cstdlib>
#include <cstring>

const size_t buffer_len = 1000001;

char temp[buffer_len] = {'\0'};

int main()
{
    int case_no = 0;
    while (true) {
        case_no++;
        char *res = fgets(temp, buffer_len, stdin);
        if (res == NULL)
            break;
        if (temp[0] == '\n')
            break;

        int n;
        scanf("%d\n", &n);

        printf("Case %d:\n", case_no);
        for (int t = 0; t < n; t++) {
            int i, j;
            scanf("%d %d\n", &i, &j);

            int min, max;
            if (i > j) {
                min = j;
                max = i;
            } else {
                min = i;
                max = j;
            }

            char cur = temp[min];
            bool same = true;
            for (int ind = min; ind <= max; ind++) {
                if (cur != temp[ind]) {
                    same = false;
                    break;
                }
            }

            printf("%s\n", same ? "Yes" : "No");
        }
    }
}

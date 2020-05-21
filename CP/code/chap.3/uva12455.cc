#include <bits/stdc++.h>

using namespace std;

int main(int argc, char *argv[])
{
    int t;
    scanf("%d\n", &t);

    int bars[20];
    while (t--) {
        int n, p;
        cin >> n >> p;
        
        for (size_t i = 0; i< p; i++)
            cin >> bars[i];

        // here i is the bitset indicating the present element in a subset of barsb
        bool possible = false;
        int i;
        for (i = 0; i < (1 << p); i++) {
            int sum = 0;
            for (size_t j = 0; j < p; j++) {
                if (i & (1 << j))
                    sum += bars[j];
            }
            if (sum == n) {
                possible = true;
                break;
            }
        }
        if (possible)
            printf("YES\n");
        else
            printf("NO\n");
    }
    return 0;
}

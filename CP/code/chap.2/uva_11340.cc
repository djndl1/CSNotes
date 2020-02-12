#include <bits/stdc++.h>

using namespace std;

int main(int argc, char *argv[])
{
    int N;

    int vals[256];
    string line;
    line.reserve(1000);
    scanf("%d\n", &N);
    while (N--) {
        for (size_t i = 0; i < 256; i++)
            vals[i] = 0;
        int K, M, payment = 0;
        scanf("%d\n", &K);
        char alpha;
        int val;
        while (K--) {
            scanf("%c %d\n", &alpha, &val);
            vals[alpha+128] = val;
        }
        scanf("%d\n", &M);
        while (M--) {
            getline(cin, line);
            for (auto c : line)
                payment += vals[c+128];
        }
        printf("%d.%.2d$\n", payment/100, payment%100);
    }

    return 0;
}

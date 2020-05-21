#include <bits/stdc++.h>

using namespace std;

bool checkConstraint(int a, int b, int c, const vector<int>& teens)
{
    if (a == b)
        return false;
    int apos, bpos;
    for (size_t i = 0; i < teens.size(); i++) {
        if (teens[i] == a)
            apos = i;
        if (teens[i] == b)
            bpos = i;
    }

    int diff = ::abs(apos - bpos);
    if (c > 0)
        return (diff <= c);
    if (c < 0)
        return (diff >= (-c));
    return false;
}

int main(int argc, char *argv[])
{
    int n, m;

    while (scanf("%d %d\n", &n, &m), (n || m)) {
        vector<int> teens(n);
        iota(teens.begin(), teens.end(), 0);
        vector<int> a(m), b(m), c(m);
        for (int i = 0; i < m; i++) {
            cin >> a[i] >> b[i] >> c[i];
        }

        size_t counter = 0;
        do {
            bool constrained = true;
            for (size_t i = 0; i < m; i++) {
                if (not checkConstraint(a[i], b[i], c[i], teens)) {
                    constrained = false;
                    break;
                }
            }
            if (constrained)
                counter++;
        } while (next_permutation(teens.begin(), teens.end()));
        printf("%d\n", counter);
    }
    return 0;
}

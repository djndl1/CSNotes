#include <iostream>
#include <string>
#include <cstdio>

using namespace std;

int main(int argc, char *argv[])
{
    int n;
    scanf("%d", &n);

    string S;
    while (n-- > 0) {
        cin >> S;

        if (S.empty())
            continue;
        if (S == "1" || S == "4" || S == "78")
            printf("+\n");
        else if (S[S.size()-1] == '5' && S[S.size()-2] == '3')
            printf("-\n");
        else if (S[0] == '9' && S[S.size()-1] == '4')
            printf("*\n");
        else if (S[0] == '1' && S[1] == '9' && S[2] == '0')
            printf("?\n");
    }
    return 0;
}

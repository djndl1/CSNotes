#include <iostream>
#include <list>
#include <string>

using namespace std;

int main()
{
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    string line{};
    while (getline(cin, line)) {
        list<char> beiju{};
        list<char> internal{};

        bool appending = true;
        for (char c : line) {
            if (c == '[' and appending) {
                appending = false;
                continue;
            }
            else if (c == '[' and not appending) {
                appending = false;
                beiju.splice(beiju.cbegin(), internal);
                continue;
            }
            else if (c == ']' and not appending) {
                appending = true;
                beiju.splice(beiju.cbegin(), internal);
                continue;
            } else if (c == ']' and appending) {
                continue;
            }

            if (appending) {
                beiju.push_back(c);
            } else {
                internal.push_back(c);
            }
        }
        if (not appending)
            beiju.splice(beiju.cbegin(), internal);


        for (char c : beiju)
            cout << c;
        cout << '\n';
    }

}

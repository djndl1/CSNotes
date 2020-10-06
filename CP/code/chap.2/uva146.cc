#include <algorithm>
#include <iostream>
#include <vector>

using namespace std;

string read_code()
{
    string tmp{};
    getline(cin, tmp);
    while (tmp.empty()) {
        getline(cin, tmp);
    }

    return tmp;
}

int main()
{
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    while (true) {
        auto seed = read_code();
        if (seed == "#")
            break;

        if (next_permutation(seed.begin(), seed.end())) {
            cout << seed << '\n';
        } else {
            cout << "No Successor" << '\n';
        }
    }
}

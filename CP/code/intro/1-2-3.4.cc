#include <iterator>
#include <algorithm>
#include <vector>
#include <iostream>

using namespace std;

int main(int argc, char *argv[])
{
    int tmp;
    vector<int> ints{};

    while (cin >> tmp) {
        ints.push_back(tmp);
    }
    sort(ints.begin(), ints.end());
    auto e = unique(ints.begin(), ints.end());
    for (auto it = ints.begin(); it != e; it++)
        cout << *it << ' ';
    cout << endl;
    return 0;
}

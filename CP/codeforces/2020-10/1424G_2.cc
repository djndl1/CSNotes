#include <algorithm>
#include <vector>
#include <map>
#include <iostream>
#include <numeric>
#include <utility>

using namespace std;

int main()
{
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    uint32_t n;
    cin >> n;
    string temp;
    getline(cin, temp);

    map<uint32_t, int64_t> incrementOfYear{};
    for (uint32_t p = 0; p < n; p++) {
        uint32_t by, dy;
        cin >> by >> dy;
        incrementOfYear[by] += 1;
        incrementOfYear[dy] -= 1;
    }

    int64_t running_sum = 0;
    int64_t max_population = 0;
    uint32_t max_year = 0;
    for (auto &inc : incrementOfYear) {
        running_sum += inc.second;
        if (running_sum > max_population) {
            max_year = inc.first;
            max_population = running_sum;
        }
    }

    cout << max_year << " " << max_population << '\n';
}

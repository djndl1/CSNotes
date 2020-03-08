#include "cut_rod.h"

#include <vector>
#include <iostream>
#include <stdexcept>
#include <tuple>
#include <iomanip>

using namespace std;


pair<vector<int>, vector<int>> bottom_up_cut_rod(const vector<int>& prices, int len)
{
    if (len > prices.size() - 1)
        throw out_of_range("Too long a rod!");
    if (len == 0)
        return make_pair<vector<int>, vector<int>>({0}, {0});

    vector<int> optimals(prices.size(), 0);
    vector<int> cuts(prices.size(), 0);
    for (size_t j = 1; j <= len; j++) {
        int optim = 0;
        for (size_t i = 1; i <= j; i++) {
            int cur_price = prices[i] + optimals[j-i];
            if (optim < cur_price) {
                optim = cur_price;
                cuts[j] = i;
            }
        }
        optimals[j] = optim;
    }
    return make_pair(optimals, cuts);
}

vector<int> cut_rod_solution(const vector<int>& prices, int len)
{
    vector<int> optimals, cuts;
    tie(optimals, cuts) = bottom_up_cut_rod(prices, len);

    vector<int> optim_cuts{};
    while (len > 0) {
        optim_cuts.push_back(cuts[len]);
        len -= cuts[len];
    }
    return optim_cuts;
}

int main(int argc, char *argv[])
{
    vector<int> optimals, cuts;
    tie(optimals, cuts) = bottom_up_cut_rod(price_table, price_table.size()-1);

    for (auto i : optimals)
        cout << i << ' ';
    cout << '\n';

    for (size_t i = 0; i < price_table.size(); i++) {
        vector<int> optim_cuts = cut_rod_solution(price_table, i);
        int sum = 0;
        for (auto i : optim_cuts) {
            sum += price_table[i];
            cout << i << ' ';
        }
        cout << ' ' << sum << ' ' << boolalpha  << (sum == optimals[i])  << '\n';
    }
    return 0;
}

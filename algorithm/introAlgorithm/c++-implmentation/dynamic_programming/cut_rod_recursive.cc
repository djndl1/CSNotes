#include "cut_rod.h"

#include <vector>
#include <iostream>
#include <utility>

using namespace std;

int cut_rod(const vector<int>& prices, int len)
{
    if (len == 0)
        return 0;
    int optimal = 0;
    for (size_t i = 1; i <= len; i++) {
        int cur_price = prices[i] + cut_rod(prices, len - i);
        optimal = (cur_price > optimal) ? cur_price : optimal;
    }
    return optimal;
}

int main(int argc, char *argv[])
{
    for (size_t i = 1; i < price_table.size(); i++) {
        cout << cut_rod(price_table, i) << ' ';
    }
    cout << '\n';
    return 0;
}

#include <iostream>
#include <algorithm>
#include <cstdint>
#include <vector>

using namespace std;

int64_t get_median(vector<int64_t> &nums)
{
    nth_element(nums.begin(), nums.begin() + nums.size() / 2, nums.end());
    if (nums.size() % 2 == 0) {
        int64_t second_median = *max_element(nums.cbegin(), nums.cbegin() + nums.size() / 2);
       return (nums[nums.size()/2] + second_median)/2;
    } else {
        return nums[nums.size()/2];
    }
}

int main()
{
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int64_t X;
    vector<int64_t> numseq{};
    numseq.reserve(10000);
    while (cin >> X) {
        numseq.push_back(X);
        cout << get_median(numseq) << '\n';
    }
}

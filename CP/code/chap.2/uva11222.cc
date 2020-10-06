#include <iostream>
#include <vector>
#include <set>
#include <algorithm>
#include <iterator>

using namespace std;

set<int> read_problems_solved()
{
    size_t S;
    cin >> S;
    set<int> problems{};
    for (size_t i = 0; i < S; i++) {
        int prblm;
        cin >> prblm;
        problems.insert(prblm);
    }

    return problems;
}

set<int> uniquely_solved(set<int> this_person, set<int> ap, set<int> bp)
{
    set<int> diff1{};

    set_difference(this_person.begin(), this_person.end(),
                   ap.begin(), ap.end(),
                   inserter(diff1, diff1.end()));


    set<int> diff2{};
    set_difference(diff1.begin(), diff1.end(),
                   bp.begin(), bp.end(),
                   inserter(diff2, diff2.end()));

    return diff2;
}

int main()
{
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int i = 0;
    int T;
    cin >> T;
    while (T--) {
        i++;

        auto first = read_problems_solved();
        auto second = read_problems_solved();
        auto third = read_problems_solved();

        auto first_unique = uniquely_solved(first, second, third);
        auto second_unique = uniquely_solved(second, first, third);
        auto third_unique = uniquely_solved(third, first, second);

        vector<set<int>> uniques(3);
        uniques[0] = first_unique;
        uniques[1] = second_unique;
        uniques[2] = third_unique;
        std::vector<int> indices(3);
        indices[0] = first_unique.size();
        indices[1] = second_unique.size();
        indices[2] = third_unique.size();

        size_t greatest = *std::max_element(indices.cbegin(), indices.cend());
        cout << "Case #" << i << ":\n";
        for (size_t p = 0; p < 3; p++) {
            if (indices[p] == greatest) {
                cout << p+1 << ' ' << greatest;
                for (auto prblm : uniques[p]) {
                    cout << ' ' << prblm;
                }
                cout << '\n';
            }
        }
    }
}

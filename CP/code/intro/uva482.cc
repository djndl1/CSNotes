#include <cstdio>

#include <vector>
#include <string>
#include <iostream>
#include <sstream>

using namespace std;

vector<string> reorder_perm(vector<string> floats, vector<size_t> indices)
{
    vector<string> reordered(floats.capacity());

    for (size_t i = 0; i < indices.size(); i++) {
        reordered[indices[i]-1] = floats[i];
    }

    return reordered;
}

int main()
{
    std::ios::sync_with_stdio(false);

    size_t cases;
    cin >> cases;
    string garbage{};
    getline(cin, garbage);

    for (size_t n_case = 0; n_case < cases; n_case++) {
        cin.ignore();
        string perm{};
        getline(cin, perm);

        vector<size_t> indices{};
        istringstream perm_ss(perm);
        size_t ind;
        while (perm_ss >> ind) {
            indices.push_back(ind);
        }

        string float_line{};
        getline(cin, float_line);
        vector<string> floats{};
        istringstream float_ss(float_line);
        string fs{};
        while (float_ss >> fs) {
            floats.push_back(fs);
        }

        if (n_case != 0)
            cout << '\n';

        auto reordered = reorder_perm(floats, indices);
        for (auto f : reordered) {
            cout << f << '\n';
        }

    }
}

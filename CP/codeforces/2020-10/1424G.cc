#include <algorithm>
#include <vector>
#include <cstdint>
#include <set>
#include <iostream>

using namespace std;

class life_time
{
    public:
        uint32_t birth_year;
        uint32_t death_year;

        life_time()
            : birth_year{}, death_year{}
        { }


        life_time(uint32_t by, uint32_t dy)
            : birth_year{by}, death_year{dy}
        { }
};

int main()
{
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    cin >> n;
    string temp;
    getline(cin, temp);

    vector<life_time> population{};
    population.reserve(n);
    set<uint32_t> check_points{};

    for (int p = 0; p < n; p++) {
        uint32_t by, dy;
        cin >> by >> dy;
        life_time person{by, dy};
        population.push_back(person);
        check_points.insert(by);
        check_points.insert(dy);
    }

    uint32_t max_year = 0;
    uint32_t max_population = 0;

    for (uint32_t cur_year : check_points) {
        uint32_t cur_population = count_if(population.begin(), population.end(),
                                           [cur_year](const life_time &p) {
                                               return p.birth_year <= cur_year and
                                                   cur_year < p.death_year;
                                           });
        if (cur_population > max_population) {
            max_year = cur_year;
            max_population = cur_population;
        }
    }
    cout << max_year << " " << max_population << '\n';
}

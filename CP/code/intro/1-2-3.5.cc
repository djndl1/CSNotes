#include <algorithm>
#include <iostream>
#include <vector>

using namespace std;

struct birthdate {
    int dd;
    int mm;
    int yyyy;
};

int main(int argc, char *argv[])
{
    int n;
    vector<birthdate> persons;

    cin >> n;
    while (n-- > 0) {
        birthdate tmp;
        cin >> tmp.yyyy >> tmp.mm >> tmp.dd;
        persons.push_back(tmp);
    }

    sort(persons.begin(), persons.end(),
         [] (const birthdate& l, const birthdate& r) -> bool {
             if (l.mm != r.mm)
                 return l.mm < r.mm;
             else if (l.dd != r.dd)
                 return l.dd < r.dd;
             else
                 return l.yyyy > r.yyyy;
         });

    for (auto p : persons)
        cout << p.mm << ' ' << p.dd <<  ' ' << p.yyyy << endl;
    return 0;
}

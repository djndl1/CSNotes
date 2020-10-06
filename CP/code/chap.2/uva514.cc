#include <iostream>
#include <stack>
#include <sstream>

using namespace std;

bool can_be_marshalled(istringstream &orders, int coach_size)
{
    stack<int> course{};

    int cur_coach = 1;
    int supposed;
    orders >> supposed;

    while (cur_coach <= coach_size) {
        while (course.size() > 0 and
               course.top() == supposed) {
            course.pop();
            orders >> supposed;
        }

        while (cur_coach != supposed) {
            course.push(cur_coach++);
        }
        course.push(cur_coach++);

    }
    return course.empty() ? true : false;
}

int main()
{
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int i = 0;
    while (true) {
        i++;

        string garbage{};
        int N;
        cin >> N;
        getline(cin, garbage);
        if (N == 0)
            break;

        if (i > 1)
            cout << '\n';
        while (true) {
            string tmp{};
            getline(cin, tmp);
            if (tmp.size() == 1 and tmp[0] == '0')
                break;

            istringstream coach_stream{tmp};
            bool marshalled = can_be_marshalled(coach_stream, N);

            cout << (marshalled ? "Yes" : "No") << '\n';
        }
    }
}

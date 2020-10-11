#include <iostream>
#include <algorithm>
#include <sstream>
#include <vector>
#include <utility>

using namespace std;

class solving_status
{
    public:
       
    bool corrected;
    int penalty;

    solving_status() = default;
};

class contestant
{
    private:
        vector<solving_status> problem_statuses;
    public:
        int contestant_number;

        int problems_solved() const
        {
            return count_if(problem_statuses.cbegin(), problem_statuses.cend(),
                            [](const solving_status& prblm) { return prblm.corrected; });
        }

        int penalty_time() const
        {
            int penalty = 0;
            for (auto& status : problem_statuses) {
                if (status.corrected)
                    penalty += status.penalty;
            }

            return penalty;
        }

        contestant() = default;

        contestant(int team_number)
            : problem_statuses(10)
        {
            contestant_number = team_number;
        }

        contestant(const contestant& other)
            : problem_statuses{other.problem_statuses},
              contestant_number(other.contestant_number)
        { }

        contestant(contestant&& other)
            : problem_statuses{std::move(other.problem_statuses)},
              contestant_number{other.contestant_number}
        { }

        contestant &operator=(const contestant& other)
        {
            problem_statuses = other.problem_statuses;
            contestant_number = other.contestant_number;

            return *this;
        }

        contestant &operator=(contestant&& other)
        {
            problem_statuses = std::move(other.problem_statuses);
            contestant_number = other.contestant_number;

            other.contestant_number = 0;

            return *this;
        }

        void add_problem(int problem_number, int time, string res)
        {
            if (not problem_statuses[problem_number].corrected) {
                if (res == "C")
                    problem_statuses[problem_number].penalty += time;
                else if (res == "I")
                    problem_statuses[problem_number].penalty += 20;
                problem_statuses[problem_number].corrected = (res == "C") ? true : false;
            }
        }

        friend bool
        operator<(const contestant& a, const contestant& b);

        friend ostream&
        operator<<(ostream& os, const contestant& team);
};

bool operator<(const contestant& a, const contestant& b)
{
    if (a.problems_solved() > b.problems_solved())
        return true;
    else if (a.problems_solved() < b.problems_solved())
        return false;
    else {
        if (a.penalty_time() < b.penalty_time())
            return true;
        else if (a.penalty_time() > b.penalty_time())
            return false;
        else
            return a.contestant_number < b.contestant_number;
    }
}

ostream& operator<<(ostream& os, const contestant& team)
{
    os << team.contestant_number << " " << team.problems_solved() << " " << team.penalty_time();

    return os;
}

class submission
{
    public:
        int contestant_number;
        int problem_number;
        int time;
        string result;
};

vector<contestant> summarize_submissions(const vector<submission>& subs)
{
    vector<contestant> teams{};
    teams.reserve(subs.size());
    for (auto& sub : subs) {
       auto res = find_if(teams.begin(), teams.end(),
                           [&sub](const contestant& team) {
                               return team.contestant_number == sub.contestant_number;
                           });
        if (res == teams.end()) {
            contestant new_team(sub.contestant_number);
            new_team.add_problem(sub.problem_number, sub.time, sub.result);
            teams.push_back(new_team);
        } else {
            res->add_problem(sub.problem_number, sub.time, sub.result);
        }
    }
    sort(teams.begin(), teams.end());

    return teams;
}

int main()
{
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int cases;
    cin >> cases;
    string garbage{};
    getline(cin, garbage);
    getline(cin, garbage);
    for (int c = 0; c < cases; c++) {
        string temp{};

        vector<submission> judge_queue{};
        judge_queue.reserve(500);
        while (getline(cin, temp)) {
            if (temp.empty())
                break;
            istringstream parser(temp);

            submission sm{};
            parser >> sm.contestant_number
                   >> sm.problem_number
                   >> sm.time
                   >> sm.result;

            judge_queue.push_back(sm);
        }
        auto ranking = summarize_submissions(judge_queue);

        if (c != 0)
            cout << '\n';
        for (auto team : ranking)
            cout << team << '\n';
    }
}

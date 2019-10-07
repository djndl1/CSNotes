#include <regex>
#include <string>
#include <iostream>

using namespace std;

int main(int argc, char *argv[])
{
    string tmp{};

    regex ann{"\\b[a-z][0-9][0-9]\\b"};
    getline(cin, tmp);

    regex_replace(ostream_iterator<char>(cout),
                  tmp.begin(), tmp.end(), ann, "***");
    return 0;
}

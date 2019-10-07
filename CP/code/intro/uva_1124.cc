#include <string>
#include <iostream>

using namespace std;

int main(int argc, char *argv[])
{
    string tmp{};
    while (getline(cin, tmp)) {
        cout << tmp << '\n';
    }
    return 0;
}

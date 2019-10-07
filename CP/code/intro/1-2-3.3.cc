#include <iostream>
#include <ctime>

using namespace std;

int main(int argc, char *argv[])
{
    int y, m, d;
    cin >> y >> m >> d;

    struct tm day = { .tm_year = y, .tm_mon = m - 1, .tm_mday = d};
    time_t tmday = mktime(&day);
    struct tm *day_m = gmtime(&tmday);
    char dayofweek[10];
    strftime(dayofweek, 10, "%A", day_m);
    cout << dayofweek << endl;
    return 0;

}


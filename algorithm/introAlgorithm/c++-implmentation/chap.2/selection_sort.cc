#include <algorithm>
#include <iostream>
#include <utility>

template <typename Iterator, typename Compare>
void
selection_sort(Iterator first, Iterator last, Compare comp)
{
    if (first == last)
        return ;

    Iterator min;
    for (auto i = first; i != last; i++) {
        min = std::min_element(i, last, comp);
        std::swap(*i, *min);
    }

    return;
}

template <typename Iterator>
void
selection_sort(Iterator first, Iterator last)
{
    if (first == last)
        return ;

    Iterator min;
    for (auto i = first; i != last; i++) {
        min = std::min_element(i, last);
        std::swap(*i, *min);
    }

    return;
}

#include <vector>
using std::vector;
using std::cout;
using std::endl;

int main(int argc, char *argv[])
{
    vector<int> A{31, 41, 59, 26, 42, 58};

    selection_sort(A.begin(), A.end());
    for ( auto i : A ) 
        cout << i << ' ';
    cout << endl;


    return 0;
}

#include <vector>
#include <algorithm>
#include <utility>

template <typename RandomIt, typename Comparer>
RandomIt _sort_partition(RandomIt first, RandomIt last, Comparer comp)
{
    RandomIt i = first - 1;
    for (RandomIt j = first; j < last - 1; j++) {
        if (comp(*j, *(last-1))) {
            i++;
            std::swap(*i, *j);
        }
    }
    std::swap(*(i+1), *(last-1));
    return (i+1);
}

template <typename RandomIt, typename Comparer>
void quick_sort(RandomIt first, RandomIt last, Comparer comp)
{
    if (first < last) {
        auto mid = _sort_partition(first, last, comp);
        quick_sort(first, mid, comp);
        quick_sort(mid+1, last, comp);
    }
}

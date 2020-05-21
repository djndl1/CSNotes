#pragma once

#include <algorithm>

template <typename RandomIt>
void merge_sort(RandomIt first, RandomIt last) {
  if (last - first > 1) {
    auto middle = first + (last - first) / 2;
    merge_sort(first, middle);
    merge_sort(middle, last);
    std::inplace_merge(first, middle, last);
  }
}

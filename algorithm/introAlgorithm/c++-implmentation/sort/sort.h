#pragma once

#include <vector>


void insertion_sort(std::vector<int> &A);

void insertion_sort_nonincreasing(std::vector<int> &A);

void merge_sort(std::vector<int> &A);

void merge_sort_lazy(std::vector<int> &A, size_t p, size_t r);

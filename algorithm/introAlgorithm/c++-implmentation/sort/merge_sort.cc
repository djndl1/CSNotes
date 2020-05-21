#include <climits>
#include <vector>
#include <iostream>

using std::vector;
using std::cout;
using std::endl;

// a naive merge, C-ism
void merge(vector<int>& A, size_t p, size_t q, size_t r)
{
    size_t n1 = q - p + 1;
    size_t n2 = r - q;
    vector<int> L(n1 + 1);
    vector<int> R(n2 + 1);

    for (size_t i = 0; i < n1; i++) {
        L[i] = A[p+i];
    }
    L[n1] = INT_MAX;

    for (size_t i = 0; i < n2; i++) {
        R[i] = A[q+i+1];
    }
    R[n2] = INT_MAX;

    size_t i = 0;
    size_t j = 0;
    for (size_t k = p; k <= r; k++) {
        if (L[i] <= R[j]) {
            A[k] = L[i];
            i++;
        } else {
            A[k] = R[j];
            j++;
        }
    }
}

void __merge_sort(vector<int>& A, size_t p, size_t r)
{
    if (p < r) {
        size_t q = (p + r) / 2;
        __merge_sort(A, p, q);
        __merge_sort(A, q+1, r);
        merge(A, p, q, r);
    }
}

void merge_sort(vector<int>& A)
{
    if (A.size()!=0)
        __merge_sort(A, 0, A.size()-1);
}

#include <algorithm>
#include <iterator>

// bad implementation to accommodate C-like interface
void merge_lazy(vector<int>& A, size_t p, size_t q, size_t r)
{
    auto zero = A.begin();
    auto start = std::next(zero, p);
    auto middle = std::next(zero, q+1);
    auto end = std::next(zero, r+1);

    vector<int> B(r - p + 1);
    std::merge(start, middle, middle, end, B.begin());

    std::copy(B.begin(), B.end(), A.begin());
}

void __merge_sort_lazy(vector<int>& A, size_t p, size_t r)
{
    if (p < r) {
        size_t q = (p + r) / 2;
        __merge_sort_lazy(A, p, q);
        __merge_sort_lazy(A, q+1, r);
        merge_lazy(A, p, q, r);
    }
}

void merge_sort_lazy(vector<int>& A)
{
    if (A.size() > 0)
        __merge_sort_lazy(A, 0, A.size()-1);
}

template <typename RandomIt>
void merge_sort(RandomIt first, RandomIt last)
{
    if (last - first > 1) {
        RandomIt middle = (first + last) / 2;
        merge_sort(first, middle);
        merge_sort(middle, last);
        std::inplace_merge(first, middle, last);
    }
}

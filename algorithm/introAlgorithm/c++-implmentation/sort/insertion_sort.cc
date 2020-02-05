#include <iostream>
#include <vector>

using std::vector;
using std::cout;
using std::cin;
using std::endl;

void insertion_sort(vector<int>& A)
{
    int key, i;
    
    for (size_t j = 1 ; j < A.size(); ++j) {
        key = A[j];
        i = j - 1;
        while ((i >= 0) and (A[i] > key)) {
            A[i+1] = A[i];
            i--;
        }
        A[i+1] = key;
    }
}

void insertion_sort_nonincreasing(vector<int>& A)
{
    int key, i;
    for (size_t j = 1 ; j < A.size(); ++j) {
        key = A[j];
        i = j - 1;
        while ((i >= 0) and (A[i] < key)) {
            A[i+1] = A[i];
            i--;
        }
        A[i+1] = key;
    }
}

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

int main(int argc, char *argv[])
{
    vector<int> A{31, 41, 59, 26, 42, 58};

    insertion_sort(A);
    for ( auto i : A ) 
        cout << i << ' ';
    cout << endl;

    auto B = A;
    insertion_sort_nonincreasing(B);
    for ( auto i : B ) 
        cout << i << ' ';
    cout << endl;

    return 0;
}

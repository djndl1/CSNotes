#include <climits>
#include <vector>
#include <iostream>

using std::vector;
using std::cout;
using std::endl;

// a naive merge
void merge(vector<int>& A, size_t p, size_t q, size_t r)
{
    size_t n1 = q - p + 1;
    size_t n2 = r - q;
    vector<int> L(n1 + 1);
    vector<int> R(n2 + 1);

    for (size_t i = 0; i < n1; i++) {
        L[i] = A[p+i];
        //       cout << L[i] << ' ';
    }
    //cout << '\n';
    L[n1] = INT_MAX;
    
    for (size_t i = 0; i < n2; i++) {
        R[i] = A[q+i+1];
        //  cout << R[i] << ' ';
    }
    //cout << '\n';
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

void merge_sort(vector<int>& A, size_t p, size_t r)
{
    if (p < r) {
        size_t q = (p + r) / 2;
        merge_sort(A, p, q);
        merge_sort(A, q+1, r);
        merge(A, p, q, r);
    }
}

int main(int argc, char *argv[])
{
    vector<int> A{31, 41, 59, 26, 42, 58};

    vector<int> B{1, 3, 5, 2, 4, 6};
    merge(B,0, 2, B.size() - 1);
    for ( auto i : B )
        cout << i << ' ';
    cout << endl;

    merge_sort(A, 0, A.size() - 1);
    for ( auto i : A )
        cout << i << ' ';
    cout << endl;

    return 0;
}

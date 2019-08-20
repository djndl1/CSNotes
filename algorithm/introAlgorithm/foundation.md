# The Role of Algorithms in Computing

TODO

# Getting Started

## Pseudocode conventions

1. indentation indicates block structure;

2. The loop counter retains its value after exiting the loop and the counter is beyond the final value. `to` incremets the counter and `downto` decrements.

3. Variables are local to the given procedure.

4. Compound data are organized into _objects_, which are composed of attributes, `A.length`. A variable representing an array or object represents a pointer/reference to the data representing the array or object. Null pointer is denoted as `NIL`.

5. Parameters are passed by value in nature.

6 `and`, `or` are short circuiting.


## An example with insertions sort

Insertion sort is an efficient algorithm for sorting a small number of elements.

###### Problem Formulation

Input: A sequence of $n$ numbers (called _keys_)  $\left\langle a_{1},a_{2},\dots,a_{n}\right\rangle$.

Output: a permutation (reordering) $\left\langle a_{1}^{\prime},a_{2}^{\prime},\dots,a_{n}^{\prime}\right\rangle $ of the input sequence s.t. $a_{1}^{\prime}\leq a_{2}^{\prime}\leq\cdots\leq a_{n}^{\prime}$.

###### Pseudocode algorithm

```c
for j = 2 to A.length
    key = A[j] // insert A[j] into the sorted sequence A[1...j-1].
    i = j - 1
    while i > 0 and A[i] > key
        A[i+1
        ] = A[i]
        i = i - 1
    A[i+i] = key
```

To show the algorithm is correct, we require that for such a loop, the properties of `A[i...j-1]` (a loop invariant, here sortedness) must satisfy:

- Initialization: it is true prior to the first iteration of the loop;

- Maintenance: If it is true before an iteration of the loop, it remains true before the next iteration;

- Termination: When the loop terminates, the invariant gives us a useful property that helps show that the algorithm is correct. (a point where mathematical induction should stop)


```cpp
// The boost implementation

template < class Iter_t, typename Compare = compare_iter < Iter_t > >
static void insert_sort (Iter_t first, Iter_t last,
                         Compare comp = Compare())
{
    //--------------------------------------------------------------------
    //                   DEFINITIONS
    //--------------------------------------------------------------------
    typedef value_iter< Iter_t > value_t;

    if ((last - first) < 2) return;


    for (Iter_t it_examine = first + 1; it_examine != last; ++it_examine)
    {
        value_t Aux = std::move (*it_examine);
        Iter_t it_insertion = it_examine;

        while (it_insertion != first and comp (Aux, *(it_insertion - 1)))
        {
            *it_insertion = std::move (*(it_insertion - 1));
            --it_insertion;
        };
        *it_insertion = std::move (Aux);
    };
};
```

## Analyzing Algorithms

Analyzing an algorithm has come to mean predicting the resources that the algorithm requires.

The worst-case running time of an algorithm gives us an upper bound on the running time for any input. For some algorithms, the worst case occurs fairly often (search a record in a database only to find no results). The average case is often roughly as bad as the worst case.

## Designing Algorithms

### Divide and Conquer

The divide-and-conquer paradigm involves three steps at each level of the recursion:

1. Divide the problem into a number of subproblems that are smaller instances of the same problem

2. Conquer the subproblems by solving them recursively. If the subproblem sizes are small enough, just solve them in a straightforward manner

3. Combine the solutions to the subproblems into the solution for the original problem

###### Merge Sort: an example

```c
MERGE(A,p,q,r): // A is the an array; A[p..q] and A[q+1..r] are in sorted order
n_1 = q - p + 1
n_2 = r - q
let L[1..n_1 + 1] and R[1..n_2 + 1] be new arrays
for i = 1 to n_1
    L[i] = A[p+i-1]
for j = 1 to n_2
   R[j] = A[q+j]
L[n_1+1] = inf
R[n_2+1] = inf

i = 1
j = 1
for k = p to r
    if L[i] <= R[j]
        A[k] = L[i]
        i = i + 1
    else
        A[k] = R[j]
        j = j + 1
```

`MERGE` procedure runs in $\Theta (n)$ time.

```c
MERGE-SORT(A,p,r)
if p < r
    q = floor((p + r) / 2)
    MERGE-SORT(A,p,q)
    MERGE-SORT(A,q+1,r)
    MERGE(A,p,q,r)
```

The running time of a recurrence can be described as 

$$
T\left(n\right)=\begin{cases}
\Theta\left(1\right) & \text{if }n\leq c,\\
aT\left(n/b\right)+D\left(n\right)+C\left(n\right) & \text{otherwise}
\end{cases}
$$

where $D(n)$ is the time to divide the problem and $C(n)$ to combine the solutions, $a$ is the number of subproblems, each with size $1/b$.

In case of merge sort:

$$
T\left(n\right)=\begin{cases}
c & \text{if }n=1,\\
2T\left(n/2\right)+cn & \text{if }n>1
\end{cases}
$$

The result is $\Theta(n\log_2 n)$.

```cpp
// implementation from libcxx, this does not create new arrays, the __result cannot be overlapped with sources. 
template <class _Compare, class _InputIterator1, class _InputIterator2, class _OutputIterator>
_OutputIterator
__merge(_InputIterator1 __first1, _InputIterator1 __last1,
        _InputIterator2 __first2, _InputIterator2 __last2, _OutputIterator __result, _Compare __comp)
{
    for (; __first1 != __last1; ++__result)
    {
        if (__first2 == __last2)
            return _VSTD::copy(__first1, __last1, __result);
        if (__comp(*__first2, *__first1))
        {
            *__result = *__first2;
            ++__first2;
        }
        else
        {
            *__result = *__first1;
            ++__first1;
        }
    }
    return _VSTD::copy(__first2, __last2, __result);
}
```

# Growth of Functions

TODO

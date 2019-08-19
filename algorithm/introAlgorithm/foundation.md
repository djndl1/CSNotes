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

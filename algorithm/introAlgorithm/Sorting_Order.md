# Intro

- Input: A sequence of $n$ numbers $\left\langle a_{1},a_{2},\dots,a_{n}\right\rangle$.

- Output: A permutation (reordering) $\left\langle a_{1}^{\prime},a_{2}^{\prime},\dots,a_{n}^{\prime}\right\rangle $ of the input sequence such that $a_{1}^{\prime}\leq a_{2}^{\prime}\leq\cdots\leq a_{n}^{\prime}$.

In practice, each number is usually part of a collection of data called _record_. Each record contains a key, which is the value to be sorted. The remainder of the record consists of _satellite data_, which are usually carried around with the key.

Sorting is considered to be the most fundamental problem in the study of algorithms. Algorithms often use sorting as a key subroutine. Many engineering issues come to the fore when implementing sorting algorithms. We can prove a nontrivial lower bound for sorting(?).

sorting algorithm summary TODO

The $i$th order statistic of a set of $n$ numbers is the $i$th smallest number in the set.

# Heapsort

Running time $O\left(n\log_{2}\left(n\right)\right)$. 

Heapsort sorts in place: only a constant number of array elements are stored outside the input array at any time. Heapsort introduces a data structure heap to manage information, which also makes an efficient priority queue.

## Heapsort
The (binary) heap data structure is an array object that we can view as a nearly complete binary tree. An array that represents a heap is an object with _length_ (the number of elements in the array) and _heap size_ (the number of elements in the heap that are stored within the array). 

The root of the tree is `A[i]`. The parent, left child, right child of a node, given its index $i$ in the array is:

- Parent: `floor(i/2)`; 

- Left: `2*i`;

- right `2*i + 1`

which can be efficiently implemented using bit shift. Any node greater than floor($n/2$) is a leaf node.

Two kinds of binary heaps:

- max-heaps: `A[Parent(i)] >= A[i]`. The largest element in a max-heap is stored at the root and the subtree rooted at a node contains values no larger than that contained at the node itself. Used for heap-sort.

- min-heaps: `A[Parent(i) <= A[i]`.  The smallest element in a min-heap is at the root. Commonly implements priority queues.

The _height_ of a node in a heap is defined to be the number of edges on the longest simple downward path from the node to a leaf. The height of a heap is the height of its root $\Theta\left(\log_{2}n\right)$. Basic operations on heaps run in time at most proportional to the height of the tree and take $O\left(\log_{2}n\right)$.

### Heapify

`MAX-HEAPIFY` maintains the max-heap property

```c
MAX-HEAPIFY(A,i)
l = LEFT(i)
r = RIGHT(i)
if l <= A.heap-size and A[l] > A[i]
    largest = l
else 
    largest = i
if r <= A.heap-size and A[r] > A[largest]
    largest = r
if largest != i
    exchange A[i] with A[largest]
    MAX-HEAPIFY(A,largest) // violation might happen after swapping
```

Running time: $T\left(n\right)\leq T\left(2n/3\right)+\Theta\left(1\right)\implies T\left(n\right)=O\left(\log_{2}n\right)$ (?)

```cpp

```

### Build a heap

```c
A.heap-size = A.length
for i = floor(A.length/2) downto 1
    MAX-HEAPIFY(A,i)
```

The loop invariant is that at the start of each iteration of the `for` loop, each node $i+1$, $i+2$, ..., $n$ is the root of max-heap.

An $n$-element heap has height `floor(log(n))`, we can build a max-heap from an unordered array in linear time ?

```cpp
// from libcxx

template <class _Compare, class _RandomAccessIterator>
void
__make_heap(_RandomAccessIterator __first, _RandomAccessIterator __last, _Compare __comp)
{
    typedef typename iterator_traits<_RandomAccessIterator>::difference_type difference_type;
    difference_type __n = __last - __first;
    if (__n > 1)
    {
        // start from the first parent, there is no need to consider children
        for (difference_type __start = (__n - 2) / 2; __start >= 0; --__start)
        {
            __sift_down<_Compare>(__first, __last, __comp, __n, __first + __start);
        }
    }
}
```

```cpp
template <class _Compare, class _RandomAccessIterator>
void
__sift_down(_RandomAccessIterator __first, _RandomAccessIterator /*__last*/,
            _Compare __comp,
            typename iterator_traits<_RandomAccessIterator>::difference_type __len,
            _RandomAccessIterator __start)
{
    typedef typename iterator_traits<_RandomAccessIterator>::difference_type difference_type;
    typedef typename iterator_traits<_RandomAccessIterator>::value_type value_type;
    // left-child of __start is at 2 * __start + 1
    // right-child of __start is at 2 * __start + 2
    difference_type __child = __start - __first;

    if (__len < 2 || (__len - 2) / 2 < __child)
        return;

    __child = 2 * __child + 1;
    _RandomAccessIterator __child_i = __first + __child;

    if ((__child + 1) < __len && __comp(*__child_i, *(__child_i + 1))) {
        // right-child exists and is greater than left-child
        ++__child_i;
        ++__child;
    }

    // check if we are in heap-order
    if (__comp(*__child_i, *__start))
        // we are, __start is larger than it's largest child
        return;

    value_type __top(_VSTD::move(*__start));
    do
    {
        // we are not in heap-order, swap the parent with it's largest child
        *__start = _VSTD::move(*__child_i);
        __start = __child_i;

        if ((__len - 2) / 2 < __child)
            break;

        // recompute the child based off of the updated parent
        __child = 2 * __child + 1;
        __child_i = __first + __child;

        if ((__child + 1) < __len && __comp(*__child_i, *(__child_i + 1))) {
            // right-child exists and is greater than left-child
            ++__child_i;
            ++__child;
        }

        // check if we are in heap-order
    } while (!__comp(*__child_i, __top));
    *__start = _VSTD::move(__top);
}
```

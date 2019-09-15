# Data  Structures

One of the basic rules concerning programming is that no routine should ever exceed a page.

An _abstract data type_ (ADT) is a set of operations. Abstract data types are mathematical abstractions. Objects such as lists, sets, and graphs along with their operations, can be viewed as abstract data types, just as integers, reals, and booleans are data types. There is no rule telling us which operations msut be supported for each ADT; this is a design decision.

## Lists, Stacks, Queues

### List
A general list of the form $A_1, A_2, A_3, \dots, A_N$. A special list of size 0 is an _empty list_.

Operations:

- `PrintList`

- `MakeEmpty`

- `Find`: returns the position of the first occurrence of a key

- `insert`/`Delete`: generally insert and delete some key from some position in the list

- `FindKth`

#### Implementation

##### Simple Array Implementation

This usually requires high overestimate, which wastes considerable space. Insertion and deletion are expensive. Simple arrays are generally not used to implement lists

##### Linked lists

It consists of a series of structures, of which each contains the element and a pointer to a structure containing its successor. A doubly linked list has a pointer to the predecessor. A circularly linked list has the last cell keep a pointer back to the first and vice versa.

For a singly linked list, `PrintList(L)` and `Find(L,Key)` takes linear time. `FindKth` takes $O(i)$ time and works by traversing down the list. `Delete()` can be executed in one pointer change. `Insert` requires a `malloc` call and two pointer maneuvers. We can keep a dummy header node, which is common practice.

###### Examples

To represent a single variable polynomial, we can use a singly linked list instead of an array. Each term in the polynomial is contained in one cell and the cells are in decreasing order of exponents.

Bucket sort: to sort N integers in a particular range, read them and count them a list of buckets, each of which represent a part of the range. Radix sort: TODO

##### Cursor Implementation

TODO

### The Stack/LIFO ADT

A stack is a list of the restriction that insertions and deletions can be performed in only one position, the end of the list, called the _top_.

The fundamental operations on a stack are _push_,  _pop_ and _top_.

#### Implementation

There can be a linked list implementation, with the stack pointer pointing to the next element of the header. An alternative implementation avoids pointers and is the more popular solution. In practice, there may be more than one stack. Using global variables to represent a stack is not a good choice.

#### Applications

TODO 

### The Queue ADT 

With a queue, insertion is done at one end, whereas deletion is performed at the other end.

The basic operations on a queue are:

- `Euqueue`: inserts an element at the end of the list (rear)

- `Dequeue`: deletes and returns the element at the start of the list (front)

Both have constant running time.

#### Implementation

linked list or circular array implementation

## Trees 

For large amounts of input, the linear access time of linked lists is prohibitive. Tree has a running time $O(\log N)$ of most operations on average.

A tree can be defined recursively. A tree is a collection of nodes. It can be empty. A tree consists of a distinguishing node $r$, the root, and zero or more nonempty subtrees $T_1, T_2, ..., T_k$, each of whose roots are connected by a directed edge from $r$. The root of each subtree is said to be a _child_ of $r$ and $r$ is the parent of each subtree root. Nodes with no children are known as _leaves_. Nodes with the same parent are _siblings_. 

A path from node $n_1$ to $n_k$ is defined as a sequence of nodes $n_1, n_2, ..., n_k$ such that $n_i$ is the parent of $n_{i+1}$ for $1 \leq i < k$. The length of this path is the number of edges on the path, namely $k-1$. There is a path of length zero from every node to itself. For any node $n_i$, the depth of $n_i$ is the length of the unique path form the root to $n_i$. If there is a path from $n_1$ to $n_2$,  then $n_1$ is an ancestor of $n_2$ and $n_2$ is a descendant of $n_1$. If $n_1 \neq n_2$, then they are proper ancestor and proper descendant. The sum of the depths of all nodes in a tree is known as the _internal path length_.

### Implementation 

One way is to keep the children of each node in a linked list of tree nodes.

```c
struct TreeNode {
    element_t element;
    pNode firstChild;
    pNode nextSibling;
};
```

### Traversals

```c
// preorder traverse a hierarchical file system
// a node is perforemd before its children are processed
ListDir(GenericFile d, int depth)
{
        if (isLegitEntry(d)) {
                printName(d, depth);
                if (isDir(d))
                        for child of d { // siblings
                                ListDir(child, depth + 1);
                        }
        }
}

ListDir("/", 0);
```

In another common method of traversing, the _postorder traversal_, the work at a node is performed after its children are evaluated.

```c
void sizeDirectory(GenericFile d)
{
        int totalSize = 0;

        if (isLegitEntry(d)) {
                totalSize = fileSize(d);
                if (isDir(d))
                        for child of d {
                                totalSize += SizeDirectory(d);
                                }
        }
        return totalSize;
}
```

### Binary Trees

A binary tree is a tree in which no node can have more than two children. The average depth is $O\left(\sqrt{N}\right)$, and for a binary search tree, the average depth is $O\left(\log N\right)$.

#### Implementation 

```c
typedef struct TreeNode *pNode;
typedef struct Element element_t;

struct TreeNode {
    element_t element;
    pNode leftChild;
    pNode rightChild;
};
```
#### Applications

##### Expression Trees

The leaves of an expression tree are operands, and the other nodes contain operators. This uses inorder traversal (normal order) or postorder (reverse polish). If we are to convert a reverse polish expreossion to a normal expression:

1. push operands (seen as trees with a single node) into the stack until an operator is encountered

2. pop the two operands before the operator and make them the children of the operator

3. continue until there is only one element in the stack 

### Binary Search Tree 

For every node X in the tree, the values of all the keys in its left subtree are smaller than the key value in X and the values of all the keys in its right subtree are larger than the key value in X.

The running time of all the operations except `makeEmpty` is $O\left(d\right)$, where $d$ is the depth of the node containing the accessed key.

Let $D\left(N\right)$ be the internal path length for some tree $T$ of $N$ nodes.

$$
D(1) = 0 \\

D\left(N\right)=D\left(i\right)+D\left(N-1-i\right)+N-1 \quad \text{for} \quad 0 \leq i < N

$$

Preorder, inorder, postorder traversals on binary search trees have $O(N)$ running time. A fourth traversal is level-order traversal.

TODO

#### AVL Trees

An AVL (Adelson-Velskii and Landis) tree is a binary search tree with a balance condition that for every node in the tree, the height of the left and right subtrees can differ by at most 1 (The heigth of an empty is defined to be $-1$). Height information is kept for each node in the node structure. The height of an AVL tree is at most roughly $1.44\log\left(N+2\right)$, but in practice only slightly more than $O\left(\log N\right)$. The minimum number of nodes 

$S
\left(h\right)=S\left(h-1\right)+S\left(h-2\right)+1
$$

where $S\left(0\right)=1,S\left(h\right)=2$. All the tree operations can be performed in $O\left(\log N\right)$, except possibly insertion, which needs to update all the balancing information for the nodes on the path back to the root. What's more, simple insertion may violate the AVL tree property. After an insertion, only nodes that are on the path from the insertion point to the root might have their balance altered.

For a tree $\alpha$, a violation might occur when an insertion into 

- the left subtree of the left child

- the right subtree of the left child

- the left subtree of the right child

- the right subtree of the right child


##### Single Rotation

For the left-left case, 

1. make the left child the new root

2. move the old root to the right child of the new root

3. move the right child of the left child to the left of the old root.

The right-right case is a symmetric case. After the rotation, the new height of the entire subtree is exactly the same as the height of the original subtree prior to the insertion.

```
                        +--+                                                            +--+
               +--------+k2+--------------+                                     +-------+k1+--------+
               |        +--+              |                                     |       +--+        |
               |                          |                                     |                   |
               |                          |                                     |                   |
               |                          |                                     |                   |
             +-++                    +----|---+                            +----|---+              ++-+
             |k1|                    |        |                            |        |         +----+k2+--------+
     +-----------------+             |   Z    |      +------------>        |        |         |    +--+        |
     |                 |             |        |                            |        |         |                |
     |                 |             +--------+                            |   X    |     +---|-+          +---|--+
     |                 |                                                   |        |     |     |          |      |
     |                 |                                                   |        |     |  Y  |          |      |
+----|--+           +--|---+                                               |        |     |     |          |  Z   |
|       |           |      |                                               |        |     |     |          |      |
|       |           |  Y   |                                               +--------+     +-----+          +------+
|       |           |      |
|  X    |           +------+
|       |
|       |
|       |
+-------+
```

```c
static pNode avl_leftSingleRotate(avlTree tree)
{
        pNode newRoot = tree->left;
        tree->left = newRoot->right;
        newRoot->right = tree;

        tree->height = max(avl_height(tree->left), avl_height(tree->right)) + 1;
        newRoot->height = max(avl_height(newRoot->left), avl_height(newRoot->right)) + 1;

        return newRoot;
}
```


##### Double Rotation

For the left-right case,

1. make the left-right node the new root,

2. move the left child of the new root to the right of the left child of the old root

3. move the right child of the new root to the right of the old root

4. make the left child of the old root the left child of the new root

5. make the old root the right child of the new root

It's actually two single rotations.

The right-left case is a symmetric case.

```
                       +---+
              +--------+k3 +--------------+
              |        +---+              |                                                  +--+
              |                           |                                         +--------+k2+--------+
              |                           |                                         |        +--+        |
            +-|-+                   +-----|---+                                     |                    |
   +--------+k1 +-----+             |         |                                     |                    |
   |        +---+     |             |         |                                   +-++                 +-++
   |                  |             |         |                              +----+k1+----+          +-+k3+------+
+--|---+            +-++            |    D    |     +--------------->        |    +--+    |          | +--+      |
|      |       +----+k2+-----+      |         |                              |            |          |           |
|      |       |    +--+     |      |         |                              |            |          |           |
|      |       |             |      |         |                           +--|--+      +--|--+    +--|--+     +--|---+
|  A   |       |             |      +---------+                           |     |      |     |    |     |     |      |
|      |       |             |                                            |     |      |     |    |     |     |      |
|      |    +--|--+       +--|-+                                          |     |      |     |    |     |     |      |
|      |    |     |       |    |                                          |  A  |      |  B  |    |  C  |     |   D  |
|      |    |     |       |    |                                          |     |      |     |    |     |     |      |
+------+    |  B  |       | C  |                                          |     |      |     |    |     |     |      |
            |     |       |    |                                          |     |      |     |    |     |     |      |
            |     |       |    |                                          +-----+      +-----+    +-----+     +------+
            |     |       |    |
            +-----+       +----+
```

```c
static pNode avl_leftDoubleRotate(avlTree tree)
{
        tree->left = avl_rightSingleRotate(tree->left);
        return avl_leftSingleRotate(tree);
}
```

```c
avlTree avl_insert(avlTree tree, element_t elem)
{
        if (tree == NULL)
                tree = avl_makeTree(elem);
        else if (element_comp(&elem, &tree->elem) < 0) {
                tree->left = avl_insert(tree->left, elem);
                if (avl_height(tree->left) - avl_height(tree->right) == 2)
                        if (element_comp(&elem, &tree->left->elem) < 0)
                                tree = avl_leftSingleRotate(tree);
                        else
                                tree = avl_leftDoubleRotate(tree);
        } else if (element_comp(&elem, &tree->right->elem) > 0) {
                tree->right = avl_insert(tree->right, elem);
                if (avl_height(tree->right) - avl_height(tree->left) == 2)
                        if (element_comp(&elem, &tree->right->elem) < 0)
                                tree = avl_rightSingleRotate(tree);
                        else
                                tree = avl_rightDoubleRotate(tree);
        }
        tree->height = max(avl_height(tree->left), avl_height(tree->right)) + 1; // important

        return tree;
}
```

### Splay Trees

When a sequence of $M$ operations has total worst-case running time of $O\left(MF\left(N\right)\right)$. A splay tree has an $O\left(\log N\right)$. 

The basic idea of the splay tree is that after a node is accessed, it is pushed to the root by a series of AVL tree rotations. By restructuring we can make future accesses cheaper on all these nodes.If the node is unduly deep, the restructuring has the side effect of balancing the tree to some extent. When a node is accessed, it is likely to be accessed again in the near future. Splay trees does not require the maintenance of height or balance information.

must read [Splaying](https://en.wikipedia.org/wiki/Splay_tree#Splaying)).

An implementation from `libgomp` [splay-tree.c](https://github.com/gcc-mirror/gcc/blob/master/libgomp/splay-tree.c).

### B-Trees

 The B-tree is a generalization of a binary search tree in that a node can have more than two children

A B-tree of order $M$ is a search tree:

- the root is either a leaf or has between $2$ or $M$ children;

- All nonleaf nodes (except the root) have between $\lceil M/2\rceil$ and $M$ children.

- All leaves are at the same depth.

All data are stored are the leaves. Every interior node (nonleaves) has pointers $P_{1,}P_{2},\dots,P_{M}$ to children and values representing the smallest key $k_{1},k_{2},\dots,k_{M-1}$ found in the subtrees $P_{2}, P_{3},\dots,P_{M}$ respectively. For every node, all the keys in subtree $P_{i-1}$ are smaller than the keys in subtree $P_{i}$. The keys act as separation values which divide its subtrees.

The leaves contains all the actual data, which are either the keys themselves or pointers to records containing the keys.

A B-tree of order is known as a 2-3-4 tree (permitted numbers of children)and a B-tree of order 3 is known as a 2-3 tree.

In order to maintain the pre-defined range, internal nodes may be joined or split. A B-tree is kept balanced after insertion by splitting a would-be overfilled node, of $2d+1$ keys, into two $d$-key siblings and inserting the mid-value key into the parent. When the split happens at the root, the tree gains depth, a new root is created.

https://github.com/gcc-mirror/gcc/blob/master/libgomp/priority_queue.h

## Hashing


The implementation of hash tables is frequently called _hashing_. Hashing is a technique used for performing insertions, deletions, and finds in constant average time. Operations that require any ordering information among the elements are not supported efficiently.

The ideal hash is an array of some fixed size containing the keys. Each key is mapped (using a hash function, ideally injective, clearly impossible, but better evenly) into some number in the range $0$ to $\text{TableSize} - 1$ and placed in the appropriate cell.

If the input keys are integers, simply returning $\text{KEY}\ mod\ \text{TableSize}$ is generally a reasonable strategy. It is usually a good idea to ensure that the table size is prime. When the input keys are random integers, then this strategy is simple to compute and distributes the keys evenly.

For string keys, one hash function can be

```c
index_t hash_str1(const char *key, int size)
{
    size_t hashval = 0;

    while (*key != '\0')
        hashval += *key++;

    return hashval % size;
}
```

With large table size, this function may not distribute the keys evenly.

```c
index_t hash_str2(const char *key, size_t size)
{
        return (key[0] + 27 * key[1] + 729 * key[2]) % size;
}

```

This function doesn't give indices large enough.

Another fairly good hash function is 

$$
\sum_{i=0}^{\text{size}-1}\text{Key}\text{{[size-i-1]}}\cdot32^{i}
$$

```c
index_t hash_str3(const char *key, size_t size)
{
        unsigned long hashval = 0;

        while (*key != 0)
                hashval = (hashval << 5) + *key++;

        return hashval % size;
}
```

A common practice in this case is not to use all the characters.

### Solving Collisions

The main programming detail is collision resolution. _Separate chaining_ is to keep a list of all elements that hash to the same value. Any scheme could be used besides linked lists to resolve the collisions; a binary search tree or even another table would work.

Separate chaining hashing has the disadvantage of requiring pointers. Open addressing hashing is an alternative to resolving collisions with linked lists. Alternative cells are tried until an empty cell is found. Cells $h_0(x), h_1(X), h_2(X),...$ are tried in successions, where $h_i(X) = (Hash(X) + F(i)) \mod\ \text{TableSize}$ with $F(0) = 0$. $F$ is the collision resolution strategy. Generally, the load factor should be below $\lambda=0.5$ for open addressing.

- linear probing: $F(i) = i$. This amounts to trying cells sequentially with wraparound in search of an empty cell. The table should be big enough. Any key that hashes into the cluster may require several attempts to resolve the collision and causes _primary clustering_. Analysis TODO

- quadratic probing: the collision function is quadratic (e.g. $F(i) = i^2$). There is no guarantee of finding an empty cell once the table gets more than half full or even before the table gets half full if the table size if not prime. If quadratic probing is used and the table size is prime, then a new element can always be inserted if the table is at least half empty (Proof TODO). Standard deletion cannot be performed in an open addressing hash table. Open addressing hash tables require lazy deletion. Quadratic probing eliminates primary clustering but introduces secondary clustering (?).

- double hashing: e.g. $F(i) = i \times hash_2(X)$. A poor choice of $hash_2(X)$ can be disastrous. It it important that all cells can be probed. $hash_2$ should never evaluate to zero. A function such as $hash_{2}(X) = R - (X \mod R)$ where $R$ is a prime smaller than TableSize.

### Rehashing

If the tables get too full, the running time for the operations will start taking too long and insertion might fail for open addressing hashing with quadratic resolution. Rehashing is a solution that builds another table that is about twice as big, with an associated new hash function, and scan down the entire original hash table, computing the new hash value for each nondeleted element and inserts it into the new table.

The running time is $O(N)$ since there are $N$ elements to rehash. Rehashing can be done when the table is half full (with quadratic probing), or when an insertion fails, or when the load factor reaches a threshold.

### Extendible Hashing

TODO

## Priority Queues (Heaps)

A priority queue has at least a `DeleteMin()` operation and an `Insert()` operation.

### Simple Implementation

A linked list, a binary search tree.

### Binary Heap

It is common for priority queue implementation to use a (binary) heap. Heaps have two properties.

#### Structure Property

a heap is a binary tree that is completely filled, with the possible exception of the bottom level, which is filled from left to right (Complete binary tree). The height of a complete binary tree is $\lfloor \log N \rfloor$. A complete binary tree can be represented in an array and no pointers are necessary. For any element in array position $i$, the left child is in position $2i$, the right child is in position $2i+1$ and the parent in position $\lfloor i/2 \rfloor$. The only problem with array implementations is that an estimate of the maximum heap size is required in advance. A heap data structure can consists of an array and an integer representing the maximum and current heap sizes.

```c
struct heap {
    size_t      capacity;
    size_t      size;
    element_t   *elems;
};
```

#### Heap Order Property

In a heap, for every node $X$, the key in the parent of $X$ is smaller than (or equal to) the key in $X$, with the exception of the root (min-heap).

#### Heap operations

- `insert`: if the element can be inserted into the pre-allocated position without violating the heap order, then we are done. Otherwise, the pre-allocated empty position bubbles up toward the root until the heap order is not violated (percolate up). 

```c
int priority_queue_insert(priority_queue_t heap, element_t elm)
{
        if (priority_queue_is_full(heap)) {
                return 1;
        }

        size_t i;
        for (i = ++heap->size;
             element_comp(&heap->elems[i/2], &elm) > 0 || i == 1;
             i /= 2)
                heap->elems[i] = heap->elems[i/2];
        heap->elems[i] = elm;

        return 0;
}
```

The time to do the insertion could be as much as $O(\log N)$, if the element to be inserted is the new minimum and is percolated all the way to the root.

```c
element_t priority_queue_delete_min(priority_queue_t heap)
{
        if (priority_queue_is_empty(heap))
                return heap->elems[0];
        element_t min = heap->elems[1];
        element_t last = heap->elems[heap->size--];

        size_t i, child;
        for (i = 1; i * 2 <= heap->size; i = child) {
                child =  i * 2;
                if ( child != heap->size &&
                     element_comp(&heap->elems[child+1], &heap->elems[child]) < 0)
                        child++;

                if (element_comp(&last, &heap->elems[child]) > 0) // since size--, last must find a position to place itself
                        heap->elems[i] = heap->elems[child];
                else
                        break;
        }
        heap->elems[i] = last;
        return min;
}
```

The worst-case and average running time for `deleteMin` is $O(\log N)$.

A minheap is of no help in finding the maximum element.

Assuming that the position of every element is known by some other method, `decreaseKey`/`increaseKey` (lower/increase the value of the key at position $P$ by a positive amount), `delete`, `buildHeap` (takes as input $N$ keys and places them into an empty heap) all run in logarithmic worst-case time.

The general algorithm of `buildHeap` is to place the $N$ keys into the tree in any order and then create a heap order.

```c
for (i = N / 2; i > 0; i--) // from the first node of the lowest level
    PercolateDown(i);       // percolate up by level or more precisely, makeHeapOrder
```

with an average running time $O(N)$ and a worst-case time $O(N \log N)$.

### Applications

- The selection problem: the input is a list of $N$ elements, which can be totally ordered. The problem is to find the $k$th largest element. The first way is to make the input a heap and perform $k$ `DeleteMin` operations. The total running time is $O(N + \logN)$. Another way is to build a set (which is implemented as a heap) of $k$ elements. The first $k$ elements are placed in the set and remaining elements are compared with the $k$th largest one by one. If one of the remaining elements is larger than the $k$the element, it is inserted into the set. The total time is $O(k + (N - k)\log k) = O(N \log k)$.

- Event Simulation: TODO

#### Heap Sort

Performing $N$ `DeleteMin` operations on a heap. The total running time is $O(N \log N)$. The main problem is that it uses an extra array. The solution is to use the cell that is right past the final element of the heap to store the popped element.

Analysis TODO

### d-Heaps

A d-heap is exactly like a binary heap except that all nodes have $d$ children. The running time of insertion is $O \log_{d} N$. There is evidence suggesting that 4-heaps may outperform binary heaps in practice.

## The Disjoint Set 



# Sorting

An inversion in an array of numbers is any ordered pair $(i, j)$ having the property that $i < j$ but $A[i] > A[j]$. It is the exactly the number of swaps that needed to be performed by insertion sort. A sorted array has no inversions.

The average number of inversions in an array of $N$ distinct numbers is $N(N-1)/4$. Any algorithm that sorts by exchanging adjacent elements require $\Omega(N^2)$ time on average.

## Insertion Sort

Insertion sort consists of $N-1$ passes. For each pass, insertion sort ensures that the element in position $0$ through $P$ are in sorted order. 


 The average running time $\Theta(N^{2})$.
 
 ```c
 void insertSort(element_t elms[], size_t n)
{
        element_t tmp;
        for (size_t i = 1; i < n; i++) {
                tmp = elms[i];

                size_t j;
                for (j = i; j > 0 && element_comp(elms[j-1], tmp) > 0; j--)
                        elms[j] = elms[j-1];
                elms[j] = tmp;
        }
}
 ```

## Shellsort

The general strategy to $h_k$ sort is for each position $i$, in $h_k, h_k+1, \dots, N-1$ place the element in the correct spot among $i, i-h_k, i-2h_k, \dots$. The action of an $h_k$-sort is to perform an insertion on $h_k$ independent subarrays. The _increment sequence_ $h_1, h_2, dots, h_t$ requires $h_1 = 1$. A popular but poor choice for increment sequence is to use the sequence $h_{t} = \lfloor N/2 \rfloor and $h_k = \lfloor h_{k=1} / 2\rfloor.

```c
void shellSort(element_t elms[], size_t n)
{
        for (size_t inc = n / 2; inc > 0; inc /= 2) {
                for (size_t i = inc; i < n; i++) {
                        element_t tmp = elms[i];

                        size_t j;
                        for (j = i; j >= inc; j -= inc)
                                if (tmp < elms[j-inc])
                                        elms[j] = elms[j-inc];
                                else
                                        break;
                        elms[j] = tmp;
                }
        }
}
```

The worst case running time of shell sort using Shell's increments is $\Theta(N^2)$. Using Hibbard's increment ($1,3,7,...,$2^k - 1$), it's $\Theta(N^{3/2})$. 

Analysis TODO

## Mergesort

Merge sort runs in $O(N \log N)$ worst-case running time, and the number of comparisons used is nearly optimal.

The fundamental operation in this algorithm is merging two sorted lists. The time to merge two sorted lists is linear. At most $N-1$ comparisons are made, where $N$ is the total number of elements. This algorithm is a classic divide-and-conquer strategy.

```c
static void merge(element_t A[], element_t tmp[],
                  size_t lpos, size_t rpos, size_t rend,
                  comp_t element_comp)
{
        size_t lend = rpos - 1;
        size_t tpos = lpos;
        size_t num = rend - lpos + 1;

        while (lpos <= lend && rpos <= rend) {
                if (element_comp(A[lpos], A[rpos]) <= 0)
                        tmp[tpos++] = A[lpos++];
                else
                        tmp[tpos++] = A[rpos++];
        }

        while (lpos <= lend) {
                tmp[tpos++] = A[lpos++];
        }

        while (rpos <= rend) {
                tmp[tpos++] = A[rpos++];
        }

        for (size_t i = 0; i < num; i++, rend--)
                A[rend] = tmp[rend];
}

static void __mergeSort(element_t A[], element_t tmp[],
                        size_t left, size_t right, comp_t element_comp)
{
        if (left < right) {
                size_t center = (left + right) / 2;
                __mergeSort(A, tmp, left, center, element_comp);
                __mergeSort(A, tmp, center+1, right, element_comp);
                merge(A, tmp, left, center+1, right, element_comp);
        }
}

int mergeSort(element_t A[], size_t n, comp_t element_comp)
{
        element_t *tmp = malloc(n * sizeof(element_t));
        if (tmp == NULL)
                return -1;
        __mergeSort(A, tmp, 0, n-1, element_comp);
        free(tmp);

        return 0;
}
```

Analysis TODO

## Quicksort

Quicksort is the fastest known sorting algorithm in practice. Its average running time is $O(N \log N)$ and worst case running time of $O(N^2)$. Quicksort is a divide-and-conquer recursive algorithm.

1. If the number of elements in $S$ is 0 or 1, then return;

2. pick any element v in S as the _pivot_. The popular choice is to use the first element as the pivot (acceptable if the input is random), which is a horrible idea. A safe course is to choose the pivot randomly. The best choice of pivot would be the median of the array. A good estimate can be obtained by picking three elements randomly and using the median of these three as a pivot. The common course is to use as pivot the median of the left, right and center elements.

3. partition S into $S_{1} = \{x\in S- {v} | x \leq v\}$ and $S_{2} = \{x\in S - {v} | x\geq v\}$. The first step is to swap the pivot with the element. Set `i` to the first element and `j` to the next-to-last. Continue to advance `i`  to a small element (relative to the pivot) and `j` to a large element and swap them until `i` and `j` cross. Then swap the pivot back in the middle. We have both `i` and `j` stop if they encounter a key equal to the pivot.

4. return quicksort($S_{1}$) followed by v followed by quicksort($S_{2}$)

The reason why quicksort is faster is that the partitioning step can actually be performed in place.

For very small arrays ($N \leq 20$) quicksort does not perform as well as insertion sort.

TODO


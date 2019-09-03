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

If the input keys are integers, simply returning $\text{KEY} mod \text{TableSize}$ is generally a reasonable strategy. It is usually a good idea to ensure that the table size is prime. When the input keys are random integers, then this strategy is simple to compute and distributes the keys evenly.

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
\sum_{i=0}^{\text{size}-1}\text{Key}\text{\ensuremath{\left[\text{size}-i-1\right]}}\cdot32^{i}
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

The main programming detail is collision resolution. _Separate chaining_ is to keep a list of all elements that hash to the same value. Any scheme could be used besides linked lists to resolve the collisions; a binary search tree or even another table would work.


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
    element_t   *elems;*
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

                if (element_comp(&last, &heap->elems[child]) > 0)
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

TODO

# Sorting

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

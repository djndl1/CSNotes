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

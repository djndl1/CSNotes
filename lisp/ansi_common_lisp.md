# Basics

`quote` = `'`: do nothing

`null` reutrn true of the empty list.

`not` returns true if its argument is false

Lisp makes no distinction between a program, a procedure and a function.

Use `load` to load a lisp program.

```lisp
(defun our-member (obj lst)
    (if (null lst)
        nil
        (if (eql obj (car lst))
            lst
            (our-member obj (cdr lst)))))
```

The most general output function in Common Lisp is `format`. The standard function for input is `read`, which a complete parser, does not just read characters and return a string.

`let` introduces mew local variables.

```lisp
(defun ask-number ()
    (format t "Please enter a number.")
    (let ((val (read)))
        (if (numberp val)
            val
            (ask-number))))

(ask-number)
```

`defparameter` creates a global variable, which is often surrounded by `**`.
`defconstant` defines global constants.
`boundp` checks if a name is bound to a global variable or constant.

The most general assignment operator is `setf`.

One of the most important advantages of functional programming is that it allows _interactive testing_.

The `do` macro is the fundamental iteration operator.

```lisp
(defun show-squares (start end)
  (do ((i start (+ i 1)))
      ((> i end) 'done)

(format t "~A ~A~%" i (* i i))))

(show-squares 1 5)
```

The above iteration has a recursive version

```lisp
(defun show-squares-recursive (i end)
  (if (> i end)
      'done
      (progn
        (format t "~A ~A~%" i (* i i))
        (show-squares (+ i 1) end))))

(show-squares-recursive 1 5)
```

To iterate through the elements of a list, use `dolist`:

```lisp
(defun our-length (lst)
  (let ((len 0))
    (dolist (obj lst)
      (setf len (+ len 1))) ;such an assignment is not unacceptable
    len))

(our-length '(1 2 3 4 5))
```

A recursive version

```lisp
(defun our-length-recursive (lst)
  (if (null lst)
      0
      (+ 1 (our-length-recursive (cdr lst)))))

(our-length-recursive '(1 2 3 4 5 6))
```

`function` returns a function object given the name of the function. Just as `'` is an abbreviation for `(quote ...)`, `#'` is for `(function ...)`.

`apply` takes a function and its arguments, and returns the result of the function with the arguments applied.

`funcall` does the same thing without having to package the arguments into a list.

lambda expression: 

```lisp
(lambda (x y)
    (+ x y))
```

In common lisp, values or objects have types, not variables. This approach is called _manifest typing_. An object always has more than one type. The builtin Common lisp types form a hierarchy of subtypes and supertypes.

# Lists

Lisp has outgrown "LISt Processor". Common Lisp is a general-purpose programming language with a wide variety of data structures.

a `cons` is a pair of pointers; the first one is the `car` and the second is the `cdr`. Conses provide a convenient representation for pairs of any type. The two havels of a cons can point to any kind of object, including conses. The `cdr` of a list is either another cons or `nil`. Lists are not a distinct kind of object, but conses linked together. Every that is not a cons is an atom. Note that `nil` is both an atom and a list. Each time `cons` is called, Lisp allocates a new piece of memory with room.

`list` builds a list; `copy-list` copies a list; `append` returns the concatenation of any number of lists.

```lisp
(defun our-copy-list (lst)
  (if (atom lst)
  lst
  (cons (car lst) (our-copy-list (cdr lst)))))
```

Conses can be considered as binary trees. CL has several built-in functions for use with trees. `copy-tree` takes a tree and returns a copy of it.

```lisp
(defun our-copy-tree (tr)
  (if (atom tr)
      tr
      (cons (our-copy-tree (car tr))
            (our-copy-tree (cdr tr)))))
```

`subst` traverses a tree

```lisp
(defun our-subst (new old tree)
  (if (eql tree old)
      new
      (if (atom tree)
          tree
          (cons (our-subst new old (car tree))
                (our-subst new old (cdr tree))))))
```


Lists are a good way to represent small sets. `member`, `member-if`, `adjoin`, `intersection`, `union`, `set-difference`

```lisp
(defun our-member-if (fn lst)
  (and (consp lst)
       (if (funcall fn (car lst))
       
           lst
           (our-member-if fn (cdr lst)))))
```

```lisp
(defun new-union (x y)
  (let ((new-lst x))
    (if (null y)
        new-lst
        (if (member (car y) x)
             (new-union new-lst (cdr y))
             (new-union (append new-lst (list (car y))) (cdr y))))))
```

Another way to think of a list is as a series of objects in a particular order. In CL, sequences include both lists and vectors. `length`

To copy part of a sequence, we use `subseq`. `reverse` returns a sequence with the same elements as its argument but in reverse order.

`sort` takes a sequence and a comparison function of two arguments. For efficiency reasons, `sort` is allowed to modify the sequence given to it as an argument.

```lisp
(defun nthmost (n lst)
  (nth (- n 1)
       (sort (copy-list lst) #'>)))
```

`some`, `every`

## a run-length compression example

```lisp
(defun n-elts (elt n)
  (if (> n 1)
      (list n elt)
      elt))

(defun compr (elt n lst)
  (if (null lst)
      (list (n-elts elt n))
      (let ((next (car lst))) ;lst is the part yet to examine
        (if (eql next elt)
            (compr elt (+ n 1) (cdr lst))
            (cons (n-elts elt n)
                  (compr next 1 (cdr lst)))))))

(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))

;;; unfold a (elt n) pair
(defun list-of (n elt)
  (if (zerop n)
      nil
      (cons elt (list-of (- n 1) elt))))

(defun uncompress (lst)
  (if (null lst)
      nil
      (let ((elt (car lst))
            (rest (uncompress (cdr lst))))
        (if (consp elt)
            (append (apply #'list-of elt)
                    rest)
            (cons elt rest)))))

(setf runned (compress '(1 1 1 0 1 0 0 0 0 1)))
(uncompress runned)
```

The representation of lists as conses makes it natural to use them as pushdown stacks. Two macros `push` and `pop` are available. `pushnew` is a variant of `push` that uses `adjoin` instead of `cons`.

## accessing a list

`nth`, `nthcdr`, `last` (zero indexed); `first` to `tenth` (one-indexed)


## Equality

`eq` tests if two arguments are the same identical object. 

`eql` = `eq` + testing if two numbers or two characters are of the same value. `eql` tells whether two objects are conceptually the same, while `eq` tells whether two objects are implementationally identical. Thus `eql` is the default comparison predicate. 

`equal` tests if two objects are structurally similar (isomorphic) objects. A rough rule of thumb is that two objects are `equal` iff their printed representations are the same.

`=` is used to compare mathematical values.

```lisp
(= 5 5.0) ==> true
(eql) 5 5.0) ==> false
```

## Mapping functions

`mapcar` returns the result of applying the function to elements taken from each list until some list runs out.

```list
(mapcar #'list '(a b c) '(1 2 3 4))
```

`maplist` calls the function on successive cdrs of the lists

```list
(maplist #'(lambda (x) x) '(a b c))

```

## Dotted lists

A proper list is either `nil` or a cons whose `cdr` is a proper list. dot notation implies a nonproper list (dotted list). In dot notation, the car and cdr of each cons are shown separated by a period.

```lisp
(defun proper-list? (x)
  (or (null x)
      (and (consp x)
           (proper-list? (cdr x)))))
```

## Assoc-lists

A list of conses are called an assoc-list or alist. Such a list could represent a set of translations.

`assoc` retrieves the pair associated with a given key:

```lisp
(setf trans '((+ . "add") (- . "substract")))
(assoc '+ trans)
```

```lisp
;;; simply iterate the list and count every element sequentially
(defun occurrences-iterative (lst)
  (let ((occurs '()))
    (dolist (elm lst)
      (let ((elm-pair (assoc elm occurs)))
      (if (null elm-pair)
          (setf occurs (append occurs (list (cons elm 1))))
          (rplacd elm-pair (+ (cdr elm-pair) 1))))
    occurs))
    
;;; count (car lst) and (cdr lst) separately
(defun occurrences-recursive (lst)
  (let ((elm (car lst)))
    (if (null elm)
        nil
        (let* ((sub-occurs (occurrences-recursive (cdr lst)))
               (elm-pair (assoc elm sub-occurs)))
          (if (null elm-pair)
              (append sub-occurs (list (cons elm 1)))
              (progn (rplacd elm-pair (+ (cdr elm-pair) 1))
                     sub-occurs))))))
```

### Shortest Path in a Graph

Given a directed graph, the neighbor of a certain node is obtained as above:

```lisp
(setf net '((a b f) (b c d) (c) (d c) (f d)))
(cdr (assoc 'b net))
```

A breadth-first search implementation

```lisp
;;; generate a list of paths that extend `path' via `node'in `net'
(defun new-paths (path node net)
    (mapcar #'(lambda (n)
        (cons n path))
    (cdr (assoc node net))))
    
;;; breadth first search to `end' in `net'
  ;;; `queue' is a list of reversed candidate paths that might lead to `end', longer paths will be appended to the back
  (defun bfs (end queue net)
    (if (null queue)
        nil     ; not found
        (let ((path (car queue)))
          (let ((node (car path)))
            (if (eql node end) ; current node is the end
                (reverse path)
                (bfs end
                     (append (cdr queue)  ; (car queue) is already searched and nothing has been found
                             (new-paths path node net))
                     net))))))
                     
(defun shortest-path (start end net)
    (bfs end (list (list start)) net))
```

## Pointers, garbage collection


Every value is conceptually a pointer. When a value is assigned to a variable or store it in a data structure, what gets stored is actua
lly a pointer to the value. When the contents of the data structure or the value of the variable is asked for, Lisp returns what it points to. For efficiency, Lisp sometimes use an immediate representation instead of a pointer.

_Automatic memory management_ is one of Lisp's most valuable features. The Lisp system maintains a segment of memory, _heap_. The function `cons` returns a newly allocated cons. Allocating memory from the heap is sometimes generically known as _consing_. Memory that is no longer needed is called _garbage_, and the scavenging operation is called _garbage collectiion_ or __GC__. Allocating storage and scavenging memory to reclaim it can be expensive compared to the routine operations of a program. It is easy to write programs that cons excessively.

# Arrays and vectors

A literal array is dentoed by `#na` where `n` is the number of dimensions in the array. 

`make-array` makes an array

```lisp
(setf arr (make-array '(2 3) :initial-element nil)) ; make a 2-by-3 array with nil values
```

A literal array is denoted by 

```lisp
#2a((b nil nil) (nil nil nil))
```

where `2` is the number of dimensions in the array.

`aref` returns an reference to an element of an array.

```lisp
(setf (aref arr 0 0 ))
```

One-dimensional array is a vector, also built by calling `vector`, literally denoted by `#(a b c)`.

```lisp
(vector "a" 'b 3)
```

`svref` (simple vector)is faster than `aref` when accessing a vector.

### an example of binary search

```lisp
(defun finder (obj vec start end)
  (let ((range (- end start)))
    (if (zerop range)
        ;; if there's only one element in the vector
        (if (eql obj (aref vec start))
           obj
           nil)
        ;; otherwise, find the middle one and compare it with obj to find the next search range
       (let ((mid (+ start (round (/ range 2)))))
         (let ((obj2 (aref vec mid)))
           (if (< obj obj2)
               (finder obj vec start (- mid 1))
               (if (> obj obj2)
                   (finder obj vec (+ mid 1) end)
                   obj)))))))   ; not in two sides then the middle one is the one

(defun bin-search (obj vec)
 (let ((len (length vec)))
   (and (not (zerop len))       ; ensure the vector is not empty and reture nil
        (finder obj vec 0 (- len 1))))) ; otherwise, return the position
```

## Strings and Characters

Strings are vectors of characters, so both sequence functions and array functiosn work on them. A constant string is denoted as a series of characters surrounded by double quotes, and an individual character c as `#\c`.

`char-code` returns the number associated with a character, `code-char` returns the character associated with a number.

`char<`, `char<=`, `char=`, `char>=`, `char>` and `char/=` (different) compare characters.

`char` access the element of string specified by index and is faster than `aref` when working on strings.

### How to replace chars in strings

```lisp
(let ((str (copy-seq "Merlin")))
  (setf (char str 3) #\k)
  str)
```
Use `equal` to compare two strings.

```lisp
(equal "fred" "Fred")
```

Common lisp provides a large number of functions for comparing and manipulating strings.

`format` is one of the way to build a string.

```lisp
(format nil "~A or ~A" "truth" "dare")
```

Use `concatenate` to join several strings.

## Sequence

In Common lisp, the tyep sequence include both lists and vectors (and therefore strings).

`remove`, `length`, `subseq`, `reverse`, `sort`, `every`, `some` are actually sequence functions.

`elt` is a function that retrieves elements of sequences of any kind.

another `mirror?` suited for `vector`

```lisp
(defun mirror? (s)
  (let ((len (length s)))
    (and (evenp len)
    ;; test head and tail one by one, 
         (do ((forward 0 (+ forward 1))
              (back (- len 1) (- back 1)))
              ;; stop condition test
             ((or (> forward back)
                  (not (eql (elt s forward)
                            (elt s back))))
             ;; forward > back means check pass.
              (> forward back))))))
```

Many sequence functions take one or more keyword arguments.

- `:key`: a function that is applied to each element of a sequence before it is considered. defualt to `identity`.

```lisp
(position 'a '((c d) (a b)) :key #'car)
```

- `:start`, `:end`: at which to start, default to `0` and `nil` respectively.

- `:from-end`: if work backwards, default `nil`

- `:test`: a two-argument comparison function. Default to `eql`.

The following function returns the second word.

```lisp
(defun second-word (str)
  (let ((p1 (+ (position #\  str) 1)))
    (subseq str p1 (position #\  str :start p1))))
```

`position-if` finds an elememnt satisfying a predicate of one argument, which, of course, cannot take `:test` keyword.

```lisp
(position-if #'oddp '(2 3 4 5))
```

`member` and `member-if` functions have similar relation. Also for `find` and `find-if`, `remove` and `remove-if`.

`remove-duplicates` preserves only the last of each occurrences of any element of a sequence. It takes all keyword five arguments listed above.

`reduce` boids down a sequence into a single value using a function with two arguments which will be called with initially the first two arguments.

```lisp
(reduce #'fn '(a b c d))
```

is equivalent to 

```lisp
(fn (fn (fn 'a 'b) 'c ) 'd)
```

```lisp
(reduce #'intersection lst1 lst2 ...)
```

takes the intersection of multiple lists

This may be used to compute a polynomial in the form

$$
3x^4 + 5x^3 + 6x^2 + 7 = x (x (x (3x + 5) + 6) +0) + 7
$$

where the function should take two coefficent $a$ and $b$ and returns $ax+b$.

```lisp
(defun polynomial-compute (lst x)
    (reduce #'(lambda (a b) (+ (* a x) b)) lst))
```

A token parser

```lisp
(defun tokens (str test start)
  "a token parser"
    (let ((p1 (position-if test str :start start)))
      (if p1
          (let ((p2 (position-if #'(lambda (c)
                                     (not (funcall test c)))
                                 str :start p1))) ;; the end of a token
            (cons (subseq str p1 p2)
                  (if p2
                      (tokens str test p2)
                      nil)))
          nil))) ;; not even a single char satisfying the test

(defun constituent (c)
  "test if a char is anything but newline and space"
    (and (graphic-char-p c)
         (not (char= c #\ ))))
```

And then a date parser

```lisp
(defun parse-date (str)
  "doc"
  (let ((toks (tokens str #'constituent 0)))
    (list (parse-integer (first toks))
          (parse-month (second toks))
          (parse-integer (third toks)))))

  (defconstant +month-names+
    #("jan" "feb" "mar" "apr" "may" "jun"
      "jul" "aug" "sep" "oct" "nov" "dec"))

  (defun parse-month (str)
    (let ((p (position str +month-names+
                       :test #'string-equal)))
      (if p
          (+ p 1)
          nil)))

  (parse-date "16 Aug 1980")
```

An integer parser

```lisp
  (defun read-integer (str)
    (if (every #'digit-char-p str)
        (let ((accum 0))
          (dotimes (pos (length str))
            (setf accum (+ (* accum 10)
                           (digit-char-p (char str pos)))))
          accum)
        nil))

```
## Structures

Similarly to C struct.

```lisp
(defstruct point
    x
    y)
```

It also implicitly defines the functions `make-point`, `point-p`, `copy-point`, `point-x` and `point-y`.

`typep` can also be used to determine the type of an object.

```lisp
(typep p 'point)
```

We can also specify default values for structure fields by enclosign the field name and a default expression in a list in the original definition.

```lisp
(defstruct polemic
    (type (progn
            (format t "What kind of polemic was it? ")
            (read)))
    (effect nil))
```

We can also control things like the way a structure is displayed and the prefix used in the names of the access functions it creates.

```lisp
(defstruct (point (:conc-name p)
                  (:print-function print-point))
    (x 0)
    (y 0))
    
(setf p (make-point :x 0 :y 0))

(defun print-point (p stream depth)
    (format stream "#<~A,~A>" (px p) (py p)))
```

The `:conc-name` argument specifies what should be concatenated to the front of the field names to make access functions for them. The `print-function` is the name of the function that should be used to print a point when it has to be displayed.

## A binary search tree example

A BST is a binary tree in which, for some ordering function `<`, the left child of each elemetn is `<` the element andthe element is `<` its right child. 

The fundamental data structure is the `node` which has three fields, one for the object stored at the node, and one each for the left and right children of the node. 

```lisp
(defstruct (node (:print-function
                    (lambda (n out d)
                      (format out "#<~A>" (node-elt n)))))
    elt (l nil) (r nil))
```

```lisp
  (defun bst-min (bst)
    (and bst
         (or (bst-min (node-l bst)) bst)))

  (defun bst-max (bst)
    (and bst
         (or (bst-max (node-r bst)) bst)))
```
A BST is either `nil` or a node whose left and right fields are BSTs.

```lisp
(defun bst-insert (obj bst <)
    (if (null bst)
        (make-node :elt obj)
        (let ((elt (node-elt bst)))
          (if (eql obj elt)
              bst
              (if (funcall < obj elt)
                  (make-node
                   :elt elt
                   :l (bst-insert obj (node-l bst) <)
                   :r (node-r bst))
                  (make-node
                   :elt elt
                   :l (node-l bst)
                   :r (bst-insert obj (node-r bst) <)))))))

  (defun bst-find (obj bst <)
    (if (null bst)
        nil
        (let ((elt (node-elt bst)))
          (if (eql obj elt)
              bst
              (if (funcall < obj elt)
                  (bst-find obj (node-l bst) <)
                  (bst-find obj (node-r bst) <))))))

```

#### `cond`: a broad `switch-case` without `break`

Returns the value of the form whose test-form evaluates to true.

```lisp
 (defun select-options ()
   (cond ((= a 1) (setq a 2))
         ((= a 2) (setq a 3))
         ((and (= a 3) (floor a 2)))
         (t (floor a 3)))) =>  SELECT-OPTIONS
```

```lisp
 (setq a 1) =>  1
 (select-options) =>  2
 a =>  2
 (select-options) =>  3
 a =>  3
 (select-options) =>  1
 (setq a 5) =>  5
 (select-options) =>  1, 2
```

#### remove an element from the BST

```lisp
(defun rperc (bst)
    (make-node :elt (node-elt (node-r bst))
               :l (node-l bst)
               :r (percolate (node-r bst))))

  (defun lperc (bst)
    (make-node :elt (node-elt (node-l bst))
               :l (percolate (node-l bst))
               :r (node-r bst)))

  (defun percolate (bst)
    (cond ((null (node-l bst))
           (if (null (node-r bst))
               nil                          ; has none
               (rperc bst)))                ; has a right subtree only
          ((null (node-r bst)) (lperc bst)) ; has a left subtree only 
          (t (if (zerop (random 2))         ; has both, random at 0 or 1
                 (lperc bst)
                 (rperc bst)))))

  (defun bst-remove (obj bst <)
    (if (null bst)
        nil
        (let ((elt (node-elt bst)))
          (if (eql obj elt)
              (percolate bst)
              (if (funcall < obj elt)
                  (make-node
                   :elt elt
                   :l (bst-remove obj (node-l bst) <)
                   :r (node-r bst))
                  (make-node
                   :elt elt
                   :l (node-l bst)
                   :r (bst-remove obj (node-r bst) < )))))))

```

An inorder traverse function

```lisp
 (defun bst-inorder-traverse (fn bst)
    (when bst
      (bst-inorder-traverse fn (node-l bst))
      (funcall fn (node-elt bst))
      (bst-inorder-traverse fn (node-r bst))))
```

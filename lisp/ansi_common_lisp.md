## Basics

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

## Lists

Lisp has outgrown "LISt Processor". Common Lisp is a general-purpose programming language with a wide variety of data structures.

a `cons` is a pair of pointers; the first one is the `car` and the second is the `cdr`. Conses provide a convenient representation for pairs of any type. The two havels of a cons can point to any kind of object, including conses. The `cdr` of a list is either another cons or `nil`. Lists are not a distinct kind of object, but conses linked together. Every that is not a cons is an atom. Note that `nil` is both an atom and a list. Each time `cons` is called, Lisp allocates a new piece of memory with room.

`list` builds a list; `copy-list` copies a list; `append` returns the concatenation of any number of lists.

```lisp
(defun our-copy-list (lst)
  if (atom lst)
  lst
  (cons (car lst) (our-copy-list (cdr lst))))
```

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

### accessing a list

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

## Pointers, garbage collection


Every value is conceptually a pointer. When a value is assigned to a variable or store it in a data structure, what gets stored is actually a pointer to the value. When the contents of the data structure or the value of the variable is asked for, Lisp returns what it points to. For efficiency, Lisp sometimes use an immediate representation instead of a pointer.

## Arrays and vectors

A literal array is dentoed by `#na` where `n` is the number of dimensions in the array. 

`make-array` makes an array

```lisp
(setf arr (make-array '(2 3) :initial-element nil)) ; make a 2-by-3 array with nil values
```

`aref` returns an reference to an element of an array.

```lisp
(setf (aref arr 0 0 ))
```

One-dimensional array is a vector, also built by calling `vector`

```lisp
(vector "a" 'b 3)
```

`svref` (simple vector)is faster than `aref` when accessing a vector.

### an example of binary search

```lisp
(defun finder (obj vec start end)
  (format t "~A~%" (subseq vec start (+ end 1)))
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

`char` is faster than `aref` when working on strings.

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

Use concatenate to join several strings.

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
    
(defun print-point (p stream depth)
    (format stream "#<~A,~A>" (px p) (py p)))
```

The `:conc-name` argument specifies what should be concatenated to the front of the field names to make access functions for them. The `print-function` is the name of the function that should be used to print a point when it has to be displayed.


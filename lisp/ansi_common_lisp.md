
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

## an example of binary search

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


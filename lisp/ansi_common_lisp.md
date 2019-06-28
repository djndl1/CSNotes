
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



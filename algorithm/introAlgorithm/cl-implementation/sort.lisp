(declaim (optimize (debug 3)))

(defun selection-sort (lst)
  (if (null lst)
      nil
      (let ((item (reduce #'min lst)))
        (cons item
              (selection-sort (remove item lst :count 1))))))

(defun insert-into (item lst)
  (if (null lst)
      (list item)
      (if (< item (car lst))
          (cons item lst)
          (cons (car lst) (insert-into item (cdr lst))))))

(defun insertion-sort (lst)
  (if (null lst)
      nil
      (insert-into (car lst)
                   (insertion-sort (cdr lst)))))

(defun sortp (lst)
  (if (or (null lst) (null (cdr lst)))
      t
      (let ((rest (cdr lst)))
        (if (<= (car lst) (car rest))
            (sortp rest)
            nil))))

(defun random-list (limit count)
  (loop for i from 1 to count collect
        (random limit)))

(defun sorted-list (count)
  (loop for i from 1 to count collect i))

(random-list 100 200)

(insertion-sort (random-list 100 200))


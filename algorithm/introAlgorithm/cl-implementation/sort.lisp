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

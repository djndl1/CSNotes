;;; min
(defun min-helper (elt lst)
  (if (null lst)
      elt
      (if (< elt (car lst))
          (min-helper elt (cdr lst))
          (min-helper (car lst) (cdr lst)))))

(defun our-min (lst)
  (if (null lst)
      nil
      (min-helper (car lst) (cdr lst))))

;;; position
(defun my-index (x y)
  (if (null y)
      nil
      (if (eql (car y) x)
          0
          (let ((z (my-index x (cdr y))))
                (and z (+ z 1))))))

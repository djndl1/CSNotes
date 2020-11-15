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
(defun new-union-r (rx ry)
  (if (null ry)
      rx
      (new-union-r (adjoin (car ry) rx) (cdr ry))))

(defun new-union (x y)
  (reverse (new-union-r (reverse x) y)))

(defun our-assoc (key alist)
  (and (consp alist)
       (let ((pair (car alist)))
         (if (eql key (car pair))
             pair
             (our-assoc key (cdr alist))))))

(defun occurrences-impl (lst dict)
  (if (null lst)
      dict
      (let* ((item (car lst))
            (cnt (count item lst)))
        (occurrences-impl (remove item lst)
                          (cons (cons item cnt) dict)))))
(defun occurrences (lst)
  (sort (occurrences-impl lst nil) #'> :key #'cdr))

(defun pos+ (lst)
  (pos+-impl nil lst 0))
(defun pos+-impl (prev remaining pos)
  (if (null remaining)
      prev
      (pos+-impl  (append prev (list (+ (car remaining) pos)))
                  (cdr remaining)
                  (+ 1 pos))))

(defun construct-end (len)
  (concatenate 'string "NIL" (make-string len :initial-element (char ")" 0))))

(defun showdots-impl (head tail-lst pos)
  (if (null tail-lst)
      (concatenate 'string head (construct-end pos))
      (showdots-impl (concatenate 'string
                                  head "(" (write-to-string (car tail-lst)) " . ")
                     (cdr tail-lst)
                     (+ 1 pos))))

(defun showdots (lst)
  (showdots-impl "" lst 0))

(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setf wins obj
                    max score))))
        (values wins max))))

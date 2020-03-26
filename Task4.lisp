; № 4 Определите функцию, порождающую по заданному натуральному числу N список, состоящий из натуральных чисел от 1 до N.

(defun reverse-el (w &optional acc)
  (cond ((null w) acc)
        ((reverse-el (cdr w) (cons (car w) acc)))
   )
)
 

(defun natur(N)
   (cond ((< N 1) nil)
         (t (cons N (natur (- N 1))))
   )
)
 
;(print(natur 4))
(print(reverse-el (natur 10)))
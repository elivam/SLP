; № 3 Определите функцию, заменяющую в исходном списке все вхождения заданного значения другим
; х - замена, repEl - заменяемое значение в списке, list - список

(defun replac (x repEl list)
   (cond ((null list) nil)
         ((equal (car list) repEl) (cons x (replac x repEl (cdr list))))
         ((cons (car list) (replac x repEl (cdr list))))
    )
)
 
(print(replac 6 1 '(1 2 3 4 5 6 6)))

;№ 20, Определите функцию ПЕРВЫЙ-АТОМ, результатом которой будет первый атом
;списка. Пример:
;> (ПЕРВЫЙ-АТОМ ’(((a b)) c d))
;A

(defun first-atom(lst)
   (cond ((atom (car lst)) (car lst))
          (t (first-atom(car lst)))
   )    
)

(print (first-atom '(((a b)) c d)))
(print (first-atom '((((a b) b)) c d)))
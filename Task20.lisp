;№ 20, Определите функцию ПЕРВЫЙ-АТОМ, результатом которой будет первый атом
;списка. Пример:
;> (ПЕРВЫЙ-АТОМ ’(((a b)) c d))
;A

(defun firstAtom(lst)
   (cond ((atom (car lst)) (car lst))
          (t (firstAtom(car lst)))
   )    
)

(print (firstAtom '(((a b)) c d)))
(print (firstAtom '((((a b) b)) c d)))
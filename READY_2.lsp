;12.05.2020
;----------------------------2 БЛОК ЗАДАЧ-----------------------------------------
;----------------------------НЕЧЕТНЫЕ-----------------------------------
;№ 3  Определите функционал (APL-APPLY f x), который применяет каждую функцию fi списка
;-------ПРИЧИНА : "(где функционалы?)"------------------------------------------------
;(f1 f2 ... fn)
;к соответствующему элементу списка
;x = (x1 x2 ... xn)
;и возвращает список, сформированный из результатов.

(defun apl-apply (functions arg-x)
  ((lambda (first-f tail-f first-x tail-x) 
      (cond ((null functions) nil)
        (t 
            (append  (cons (funcall first-f first-x) 
               (apl-apply tail-f tail-x))
            )
         )
      )
   ) (car functions) (cdr functions) (car arg-x) (cdr arg-x)          
  )          
)
 

(defun mult-two (x)
  (* x 2)
)
 
(print (apl-apply '(car cadr mult-two) '((10 11 34) ( 17 78 89 ) 1 )))

(print (apl-apply '(mult-two caddr mult-two car) '(90 (10 11 34)  17  (13 12) 11)))
; ----------------------------------------------------------------------
; № 13 Определите функцию, которая возвращает в качестве значения свое определение (лямбда-выражение).

(defun self ()
	(caddr (cadddr (function-lambda-expression `self))))

(print (self))

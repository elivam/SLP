
;----------------------------2 БЛОК ЗАДАЧ-----------------------------------------
;----------------------------НЕЧЕТНЫЕ-----------------------------------
;1  Определите FUNCALL через функционал APPLY
;(funcall 'car '(1 2 3)) =>  1

(defun my-funcall (function &rest arg)
	(apply function arg)
)
	                   
(print(my-funcall 'car '(0 f 7 2 3)))
;-------------------------------------------------------------------------
;3 Определите функционал (APL-APPLY f x), который применяет каждую функцию fi списка
;(f1 f2 ... fn)
;к соответствующему элементу списка
;x = (x1 x2 ... xn)
;и возвращает список, сформированный из результатов.

(defun apl-apply (functions arg-x)
	( (lambda (head-f tail-f first-x tail-x)
				(cond
					((null head-f) nil)
					(t (append 
							(list (apply head-f (list first-x )))
							(apl-apply tail-f tail-x)
						)
					)
				)
		) (car functions) (cdr functions) (car arg-x) (cdr arg-x)
	)
)
(defun mult-two (x)
  (* x 2)
)
 
(print (apl-apply '(car cadr add-one) '((10 11 34) ( 17 78 89 ) 70 1 2)))
;-------------------------------------------------------------------------
;9 Напишите генератор порождения чисел Фибоначчи: 0, 1, 1, 2, 3, 5, ...

(defun make-fibonachi-numbers-counter ()
    (let ((f1 0) (f2 1))
        (lambda ()
            (setq v f1
                f1 (+ f2 f1)
                f2 v
             )
         )
    )
)

(setq fib (make-fibonachi-numbers-counter))

(print (funcall fib))
(print (funcall fib))
(print (funcall fib))
(print (funcall fib))
(print (funcall fib))
(print (funcall fib))

;21.04.2020
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
;-----------------------------------------------------------------------
;28.04.2020
;№ 5 Определите функциональный предикат (НЕКОТОРЫй пред список), 
;который истинен, когда, являющейся функциональным аргументом 
;предикат пред истинен хотя бы для одного элемента списка список.

(defun some-pred-list (value) 
    (cond ((= value 1) (list t)) 
        (t nil)
    )
)

(defun mapcan-fun (fun arg)
		 (not(not(mapcan fun arg)))
) 

(print (mapcan-fun 'some-pred-list '(1 1 3 4 7)))
;-----------------------------------------------------------------------
;№ 7 Определите фильтр (УДАЛйЬ-ЕСЛИ-НЕ пред список), удаляющий из списка список
;все элементы, которые не обладают свойством, наличие которого проверяет
;предикат пред.

(defun delet-if-not (pred lst)
	(mapcan pred lst)
)

(defun pred-check-not-eq-setf-arg (arg)
	(cond ((null (get arg 'prop)) nil)
		(t (list arg))
	)
)

(defun set-prop(sym)
 (setf (get sym 'prop) 'sym)
) 

(set-prop 'A)
(set-prop 'K)
(set-prop 'M)

(print (delet-if-not 'pred-check-not-eq-setf-arg '(C A K Z A L M M)))
